{-# LANGUAGE
    DeriveGeneric
  , ScopedTypeVariables
  , FlexibleContexts
  , PackageImports
  #-}

module Main where

import LText.Parser.Document
import LText.Parser.Expr
import LText.Renderer
import LText.Internal.Classes
import LText.Internal.Expr
import LText.Internal.Inference
import LText.Internal.Evaluation

import Options.Applicative
import qualified Data.Yaml as Y
import qualified Data.Aeson.Types as A

import System.Directory (doesFileExist)
import GHC.Generics

import Text.Parsec hiding (optional, ParseError)
import qualified Text.Parsec as P
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

import Data.Maybe
import Data.Monoid
import Data.Default
-- import "compositon-extra" Data.Functor.Composition
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except (runExceptT)

data AppOpts = AppOpts
  { output :: Maybe FilePath
  , left :: Maybe String
  , right :: Maybe String
  , typeQuery :: Bool
  } deriving (Eq, Show, Generic)

instance Default AppOpts where
  def = AppOpts Nothing Nothing Nothing False

instance Monoid AppOpts where
  mempty = def
  (AppOpts x l r t) `mappend` (AppOpts y l' r' t') = AppOpts (getLast $ Last y <> Last x)
                                                             (getLast $ Last l <> Last l')
                                                             (getLast $ Last r <> Last r')
                                                             (getAny $ Any t <> Any t')

instance Y.ToJSON AppOpts where
  toJSON = A.genericToJSON A.defaultOptions

instance Y.FromJSON AppOpts where
  parseJSON = A.genericParseJSON A.defaultOptions


data Desitnation = Stdout
                 | File FilePath
  deriving (Eq, Show)

data Env = Env
  { outputDest :: Desitnation
  , leftDelim  :: Maybe String
  , rightDelim :: Maybe String
  , isTypeQuery :: Bool
  } deriving (Eq, Show)


makeEnv :: AppOpts -> Env
makeEnv (AppOpts Nothing l r t)  = Env Stdout l r t
makeEnv (AppOpts (Just f) l r t) = Env (File f) l r t

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
  <$> optional ( strOption $
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
  <*> switch (
          long "type"
       <> short 't')

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
       <> header "ltext - higher-order file applicator" )

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

  (yamlConfig :: AppOpts) <- either (putStrLn . Y.prettyPrintParseException >> return def)
                                    return eYamlConfig

  runReaderT (entry $ expression runtimeOpts) $
    makeEnv $ yamlConfig <> options runtimeOpts

-- | Entry point, post options parsing
entry :: ( MonadIO m
         , MonadReader Env m
         ) => String -> m ()
entry e = do
  e' <- runExceptT $ makeExpr e
  let mainExpr = case e' of
        Left err -> error err
        Right expr -> expr

  fileExprs <- liftIO $ mapM (\f -> do
                  content <- liftIO $ LT.readFile f
                  eContentExpr <- runExceptT $ parseDocument f content
                  return $ fromError eContentExpr
                ) $ Set.toList $ fv mainExpr
  -- (fileExprs :: [Exp])  fileExprs'
  l <- leftDelim <$> ask
  r <- rightDelim <$> ask

  let subst :: Map.Map String Exp
      subst = Map.fromList $ Set.toList (fv mainExpr) `zip` fileExprs
      expr = apply subst mainExpr

  app <- ask

  if isTypeQuery app
  then liftIO $ test expr
  else liftIO $ LT.putStr $ render (l,r) expr
  where
    printErr :: MonadIO m => [Exp] -> Either P.ParseError Exp -> m [Exp]
    printErr acc (Left err) = liftIO $ print err >> return acc
    printErr acc (Right expr) = return $ expr : acc

    printErrs :: MonadIO m => [Either P.ParseError Exp] -> m [Exp]
    printErrs = foldM printErr []

    fromError me = case me of
      Left err -> error err
      Right e -> e
