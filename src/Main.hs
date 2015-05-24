{-# LANGUAGE
    DeriveGeneric
  , ScopedTypeVariables
  , FlexibleContexts
  #-}

module Main where

import LText.Parser.Document
import LText.Internal.Classes
import LText.Internal.Expr

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
import qualified Data.Set as Set
import Control.Applicative
import Control.Monad.Reader

data AppOpts = AppOpts
  { output :: Maybe FilePath
  , left :: Maybe String
  , right :: Maybe String
  } deriving (Eq, Show, Generic)

instance Default AppOpts where
  def = AppOpts Nothing Nothing Nothing

instance Monoid AppOpts where
  mempty = def
  (AppOpts x l r) `mappend` (AppOpts y l' r') = AppOpts (getLast $ Last y <> Last x)
                                                        (getLast $ Last l <> Last l')
                                                        (getLast $ Last r <> Last r')

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
  } deriving (Eq, Show)


makeEnv :: AppOpts -> Env
makeEnv (AppOpts Nothing l r)  = Env Stdout l r
makeEnv (AppOpts (Just f) l r) = Env (File f) l r

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
  (mainExpr:[]) <- printErr [] $ parse parseExpr "" $ LT.pack e

  exprs <- liftIO $ mapM (\f -> LT.readFile f >>= return . parse (parseDocument f) f) $
             Set.toList $ fv mainExpr
  (exprs' :: [Exp]) <- printErrs exprs
  l <- leftDelim <$> ask
  r <- rightDelim <$> ask

  liftIO $ print $ render (l,r) $ head exprs'
  where
    printErr :: MonadIO m => [Exp] -> Either P.ParseError Exp -> m [Exp]
    printErr acc (Left err) = liftIO $ print err >> return acc
    printErr acc (Right expr) = return $ expr : acc

    printErrs :: MonadIO m => [Either P.ParseError Exp] -> m [Exp]
    printErrs = foldM printErr []
