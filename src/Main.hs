{-# LANGAUGE GeneralizedNewtypeDeriving #-}

module Main where

import Main.Internal
import Options.Applicative

-- Main entry point
-- TODO:
-- - Maybe String => stdout | file to write to
-- - Directory searching to match expression literals

entry :: App -> IO ();
entry app = print $ "Running... " ++ show app

main :: IO ()
main = execParser opts >>= entry
  where
    opts = info (helper <*> parseApp)
      ( fullDesc
     <> progDesc "λtext Evaluator"
     <> header "λtext - Lambdas as Templates" )
