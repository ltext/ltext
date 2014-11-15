{-# LANGAUGE GeneralizedNewtypeDeriving #-}

module Main where

import Main.Internal

import Options.Applicative


data App = App { override :: Maybe Override
               , mode :: Mode }
                 deriving (Show, Eq)

data Override = Override { unparsed :: String }
                           deriving (Show, Eq)

parseOverride :: Parser Override
parseOverride = Override
             <$> strOption
               ( metavar "EXT FILE"
              <> help "Override file extension EXT with formatting rules defined in FILE" )

data Mode = Eval EvalOptions
          | Type TypeOptions
          | Check CheckOptions
            deriving (Show, Eq)



data EvalOptions = EvalOptions { expression :: String
                               , outfile :: Maybe String }
                                 deriving (Show, Eq)

evalOptions :: Parser EvalOptions
evalOptions = EvalOptions
  <$> strOption
      ( long "expr"
     <> short 'x'
     <> metavar "EXPR"
     <> help "Expression to Parse" )
  <*> ( optional $ strOption
      ( long "output"
     <> short 'o'
     <> metavar "OUT"
     <> help "redirect output to file OUT" ))

data TypeOptions = TypeOptions { file :: String }
  deriving (Show, Eq)

typeOptions :: Parser TypeOptions
typeOptions = TypeOptions
  <$> strOption
    ( metavar "FILE"
   <> help "File to query" )

data CheckOptions = CheckOptions { files :: String }
  deriving (Show, Eq)

checkOptions :: Parser CheckOptions
checkOptions = CheckOptions
  <$> strOption
    ( metavar "FILES..."
   <> help "Files to check" )



parseMode :: Parser Mode
parseMode = subparser $
            command "eval"
              ( info (Eval <$> evalOptions)
                ( progDesc "Evaluate an expression" ))
         <> command "type"
              ( info (Type <$> typeOptions)
                ( progDesc "Query a file for its type signature" ))
         <> command "check"
              ( info (Check <$> checkOptions)
                ( progDesc "Check the integrity of files" ))


parseApp :: Parser App
parseApp = App
  <$> optional parseOverride
  <*> parseMode


entry :: App -> IO ();
entry app = print $ "Running... " ++ show app

main :: IO ()
main = execParser opts >>= entry
  where
    opts = info (helper <*> parseApp)
      ( fullDesc
     <> progDesc "λtext evaluator"
     <> header "λtext - Lambdas as Templates" )
