{-# LANGUAGE
    DeriveGeneric
  , ScopedTypeVariables
  , StandaloneDeriving
  , DeriveAnyClass
  #-}

module Main where

import Options.Applicative
import qualified Data.Yaml as Y
import qualified Data.Aeson.Types as A

import System.Directory (doesFileExist)
import GHC.Generics

import Data.Maybe
import Data.Monoid
import Data.Default
import Control.Applicative
import Control.Monad.Reader

data Desitnation = Stdout
                 | File FilePath
  deriving (Eq, Show)

data AppOpts = AppOpts
  { output :: Maybe FilePath
  } deriving (Eq, Show, Generic)

instance Default AppOpts where
  def = AppOpts Nothing

instance Monoid AppOpts where
  mempty = AppOpts Nothing
  (AppOpts x) `mappend` (AppOpts y) = AppOpts $ getLast $ Last y <> Last x

instance Y.ToJSON AppOpts where
  toJSON = A.genericToJSON A.defaultOptions

instance Y.FromJSON AppOpts where
  parseJSON = A.genericParseJSON A.defaultOptions

data Env = Env
  { outputDest :: Desitnation
  } deriving (Eq, Show)


makeEnv :: AppOpts -> Env
makeEnv (AppOpts Nothing)  = Env Stdout
makeEnv (AppOpts (Just f)) = Env $ File f

-- | Command-line options - all other options, but also a way to declare the
-- location of the config file.
data App = App
  { expression :: String
  , options :: AppOpts
  , configLocation :: Maybe FilePath
  } deriving (Eq, Show)

-- | OptParse normal options
appOpts :: Parser AppOpts
appOpts = AppOpts
  <$> (optional $ strOption
        ( long "output"
       <> short 'o'
       <> metavar "OUTPUT"
       <> help "output destination" ))

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

  runReader entry $ makeEnv $ yamlConfig <> options runtimeOpts

-- | Entry point, post options parsing
entry :: Reader Env (IO ())
entry = do
  -- query the environment
  ex <- outputDest <$> ask
  return $ print ex
