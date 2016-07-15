{-# LANGUAGE
    DeriveGeneric
  , ScopedTypeVariables
  , FlexibleContexts
  #-}

module Main where

import Application.Types
import LText.Expr
import LText.Type
import LText.Document
import LText.Eval

import Options.Applicative
import System.IO
import System.Exit
import qualified Data.HashSet        as HS
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as LT
import qualified Data.Text.Lazy.IO   as LT
import Control.Monad.Reader
import Control.Monad.Catch



versionString :: String
versionString = "0.1.0"


data Opts = Opts
  { expression :: String
  , version    :: Bool
  , type'      :: Bool
  , verbose    :: Bool
  , raw        :: [FilePath]
  , leftDelim  :: Maybe String
  , rightDelim :: Maybe String
  } deriving (Eq, Show)


opts :: Parser Opts
opts =
  let expressionOpt = argument str $
           metavar "EXPRESSION"
      versionOpt = switch $
           long "version"
        <> help "Print the version number"
      typeOpt = switch $
           long "type"
        <> short 't'
        <> help "Perform type inference on an expression"
      verboseOpt = switch $
           long "verbose"
        <> short 'v'
        <> help "Be verbose, sending info through stderr"
      rawOpt = many . strOption $
           long "raw"
        <> short 'r'
        <> metavar "FILE"
        <> help "Treat these files as plaintext without an arity header"
      leftOpt = optional . strOption $
           long "left"
        <> metavar "LEFT"
        <> help "The left delimiter to use for rendering partially applied files"
      rightOpt = optional . strOption $
           long "right"
        <> metavar "RIGHT"
        <> help "The right delimiter to use for rendering partially applied files"
  in  Opts <$> expressionOpt
           <*> versionOpt
           <*> typeOpt
           <*> verboseOpt
           <*> rawOpt
           <*> leftOpt
           <*> rightOpt


optsToEnv :: Opts -> IO Env
optsToEnv (Opts ex _ t _ r ld rd) = do
  e <- runParse $ T.pack ex
  pure $ Env e t (HS.fromList r) ((,) <$> ld <*> rd)


main :: IO ()
main = do
  let cli :: ParserInfo Opts
      cli = info (helper <*> opts) $
          fullDesc
       <> progDesc "Evaluate EXPRESSION and send the substitution to stdout.\
                  \ See http://ltext.github.io/ for more details."
       <> header "λtext - higher-order file applicator"

  os <- execParser cli
  if version os
  then do hPutStrLn stderr $ "Version: " ++ versionString
          exitSuccess
  else do env <- optsToEnv os
          let catchErrs = handle handleParseError
                        . handle handlePrintError
                        . handle handleTypeError
          catchErrs $ runAppM env entry


-- | Entry point, post options parsing
entry :: ( MonadApp m
         ) => m ()
entry = do
  env <- ask
  let typeEnv = toTypeEnv env
  t <- liftIO . runTypeCheckM typeEnv . typeOfTopLevel $ topLevelExpr env
  if isTypeQuery env
  then liftIO $ do
    putStrLn $ ppType t
    exitSuccess
  else do
    e <- resolveTopLevelExpr $ topLevelExpr env
    d <- toDocument $ evaluate e
    let ds = case delims env of
               Nothing -> Nothing
               Just (ld,rd) -> Just (LT.pack ld, LT.pack rd)
    txt <- printDocument ds d
    liftIO $ do
      LT.putStr txt
      exitSuccess


-- | Translates free variables into filenames, and fetches them
resolveTopLevelExpr :: MonadApp m => Expr -> m Expr
resolveTopLevelExpr e =
  foldM go e $ freeVars e
  where
    go e' f = do
      isRaw <- HS.member f . rawTerms <$> ask
      d     <- liftIO $ if isRaw
                        then rawDocument f
                        else fetchDocument f :: IO Expr
      pure $ substitute f d e'


