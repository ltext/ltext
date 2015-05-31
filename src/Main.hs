{-# LANGUAGE
    DeriveGeneric
  , ScopedTypeVariables
  , FlexibleContexts
  #-}

module Main where

import LText.Parser.Document
import LText.Parser.Expr
import LText.Renderer
import LText.Internal

import Options.Applicative
import qualified Data.Yaml as Y
import qualified Data.Aeson.Types as A

import System.Directory (doesFileExist)
import GHC.Generics

import qualified Text.Parsec as P
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

import Data.Maybe
import Data.Monoid
import Data.Default
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except (runExceptT)

data AppOpts = AppOpts
  { typeQuery :: Bool
  , output :: Maybe FilePath
  , left :: Maybe String
  , right :: Maybe String
  } deriving (Eq, Show, Generic)

instance Default AppOpts where
  def = AppOpts False Nothing Nothing Nothing

instance Monoid AppOpts where
  mempty = def
  (AppOpts t x l r) `mappend` (AppOpts t' y l' r') = AppOpts (getAny $ Any t <> Any t')
                                                             (getLast $ Last y <> Last x)
                                                             (getLast $ Last l <> Last l')
                                                             (getLast $ Last r <> Last r')

instance Y.ToJSON AppOpts where
  toJSON = A.genericToJSON A.defaultOptions

instance Y.FromJSON AppOpts where
  parseJSON = A.genericParseJSON A.defaultOptions


data Desitnation = Stdout
                 | File FilePath
  deriving (Eq, Show)

-- | Warning - partial function
getFilePath :: Desitnation -> FilePath
getFilePath (File s) = s

data Env = Env
  { isTypeQuery :: Bool
  , outputDest :: Desitnation
  , leftDelim  :: Maybe String
  , rightDelim :: Maybe String
  } deriving (Eq, Show)


makeEnv :: AppOpts -> Env
makeEnv (AppOpts t Nothing l r)  = Env t Stdout l r
makeEnv (AppOpts t (Just f) l r) = Env t (File f) l r

-- | Command-line options - all other options, but also a way to declare the
-- location of the config file.
data App = App
  { expression     :: String
  , options        :: AppOpts
  , configLocation :: Maybe FilePath
  } deriving (Eq, Show)

-- | OptParse normal options
appOpts :: Parser AppOpts
appOpts = AppOpts
  <$> switch (
          long "type"
       <> short 't'
       <> help "query the type signature of an expression")
  <*> optional ( strOption $
          long "output"
       <> short 'o'
       <> metavar "OUTPUT"
       <> help "output destination" )
  <*> optional ( strOption $
          long "left"
       <> short 'l'
       <> metavar "LEFTDELIM"
       <> help "left delimiter" )
  <*> optional ( strOption $
          long "right"
       <> short 'r'
       <> metavar "RIGHTDELIM"
       <> help "right delimiter" )

-- | OptParse for command-line specific options
app :: Parser App
app = App
  <$> argument str (metavar "EXPRESSION")
  <*> appOpts
  <*> optional (strOption $
         long "config"
      <> short 'c'
      <> metavar "CONFIG"
      <> help "location of config file" )

main :: IO ()
main = do
  let opts :: ParserInfo App
      opts = info (helper <*> app)
        ( fullDesc
       <> progDesc "Evaluate EXPRESSION and send to OUTPUT"
       <> header "λtext - higher-order file applicator" )

  (runtimeOpts :: App) <- execParser opts

  let yamlConfigPath = fromMaybe "./.ltext/config.yaml" $
                         configLocation runtimeOpts

  yamlConfigExist   <- doesFileExist yamlConfigPath
  yamlConfigContent <- if yamlConfigExist
    then readFile yamlConfigPath
    else return ""

  (eYamlConfig :: Either Y.ParseException AppOpts) <-
    if yamlConfigExist && not (null yamlConfigContent)
    then Y.decodeFileEither yamlConfigPath
    else return $ Right def

  (yamlConfig :: AppOpts) <- either (\err -> do putStrLn $ Y.prettyPrintParseException err
                                                return def)
                                    return eYamlConfig

  runReaderT (entry $ expression runtimeOpts) $
    makeEnv $ yamlConfig <> options runtimeOpts

-- | Entry point, post options parsing
entry :: ( MonadIO m
         , MonadReader Env m
         ) => String -> m ()
entry e = do
  eitherMainExpr <- runExceptT $ makeExpr e
  let mainExpr = fromError eitherMainExpr

  fileExprs <- liftIO $ forM (Set.toList $ fv mainExpr) (\f -> do
                  content <- liftIO $ LT.readFile f
                  eContentExpr <- runExceptT $ parseDocument f content
                  return $ fromError eContentExpr)

  app <- ask

  let subst :: Map.Map String Expr
      subst = Map.fromList $ Set.toList (fv mainExpr) `zip` fileExprs
      rawExpr = apply subst mainExpr
      l = leftDelim app
      r = rightDelim app

  eitherExprType <- runExceptT $ runTI $ typeInference (Context Map.empty) rawExpr
  let exprType = fromError eitherExprType

  eitherExpr <- runExceptT $ runEv $ reduce rawExpr
  let expr = fromError eitherExpr

  if isTypeQuery app
  then liftIO $ test expr
  else if not (litsAtTopLevel expr)
       then error $ "Error: result has literals in sub expression - `" ++ show expr ++ "`."
       else if outputDest app == Stdout
            then liftIO $ LT.putStr $ render (l,r) expr
            else liftIO $ LT.writeFile (getFilePath $ outputDest app) $ render (l,r) expr
  where
    fromError me = case me of
      Left err -> error err
      Right e  -> e
