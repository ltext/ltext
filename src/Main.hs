{-# LANGUAGE
    DeriveGeneric
  , ScopedTypeVariables
  #-}

module Main where

import Options.Applicative
import qualified Data.Yaml as Y
import qualified Data.Aeson.Types as A

import System.Directory (doesFileExist)
import GHC.Generics

import Data.Maybe
import Data.Monoid
import Control.Applicative
import Control.Monad.Reader

-- | We should keep the record field names the same as the long description for
-- options.
data AppOpts = AppOpts
  { example :: String }
  deriving Generic

-- | We make a Monoid instance, simply for overwriting values.
instance Monoid AppOpts where
  mempty = AppOpts ""
  (AppOpts _) `mappend` (AppOpts y) = AppOpts y

instance Y.ToJSON AppOpts where
  toJSON = A.genericToJSON A.defaultOptions

instance Y.FromJSON AppOpts where
  parseJSON = A.genericParseJSON A.defaultOptions

-- | The value we pass around our application as a @Reader@ prefix.
data Env = Env
  { exampleEnv :: String }

-- | Choose which & how options become visible to the whole application
makeEnv :: AppOpts -> Env
makeEnv (AppOpts e) = Env e

-- | Command-line options - all other options, but also a way to declare the
-- location of the config file.
data App = App
  { options :: AppOpts
  , configLocation :: Maybe String }

-- | OptParse normal options
appOpts :: Parser AppOpts
appOpts = AppOpts
  <$> strOption
        ( long "example"
       <> short 'e'
       <> metavar "STRING"
       <> help "an example option" )

-- | OptParse for command-line specific options
app :: Parser App
app = App
  <$> appOpts
  <*> ( optional $ strOption
       ( long "config"
      <> short 'c'
      <> metavar "CONFIG"
      <> help "location of config file" ))

main :: IO ()
main = do
  (commandConfig :: App) <- execParser opts

  let yamlConfigPath = fromMaybe
        "config/config.yaml" $
        configLocation commandConfig

  yamlConfigExist <- doesFileExist yamlConfigPath
  yamlConfigContent <- if yamlConfigExist
    then readFile yamlConfigPath
    else return ""

  (mYamlConfig :: Maybe AppOpts) <- if yamlConfigExist && yamlConfigContent /= ""
        then Y.decodeFile yamlConfigPath
        else return Nothing

  let yamlConfig = fromMaybe
        mempty
        mYamlConfig

  runReader entry $ makeEnv $ yamlConfig <> options commandConfig

  where
    opts :: ParserInfo App
    opts = info (helper <*> app)
      ( fullDesc
     <> progDesc "Print out STRING"
     <> header "foo - a haskell application" )


-- | Entry point, post options parsing
entry :: Reader Env (IO ())
entry = do
  -- query the environment
  ex <- exampleEnv <$> ask
  return $ print ex
