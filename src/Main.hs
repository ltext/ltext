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
import Data.Monoid
import System.IO
import System.Exit
import qualified Data.HashSet      as HS
import qualified Data.Text         as T
import qualified Data.Text.Lazy    as LT
import qualified Data.Text.Lazy.IO as LT
import Control.Monad.Reader
import Control.Monad.Catch
import Control.Monad.IO.Class



versionString :: String
versionString = "1.0.0"


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
                  \ Notice how the filenames used CAN'T use spaces ` ` due to\
                  \ its representation as function application. It's also ghotty :x"
       <> header "λtext - higher-order file applicator"

  os <- execParser cli
  if version os
  then do hPutStrLn stderr $ "Version: " ++ versionString
          exitSuccess
  else do env <- optsToEnv os
          runAppM env entry


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
    let e = t `seq` evaluate (topLevelExpr env)
    d   <- toDocument e
    let ds = case delims env of
               Nothing -> Nothing
               Just (ld,rd) -> Just (LT.pack ld, LT.pack rd)
    txt <- printDocument ds d
    liftIO $ do
      LT.putStrLn txt
      exitSuccess


--   eitherMainExpr <- runExceptT $ makeExpr e
--   let mainExpr = fromError eitherMainExpr
-- 
--   fileExprs <- liftIO $ forM (Set.toList $ fv mainExpr) $ \f -> do
--                   content <- liftIO $ LT.readFile f
--                   eContentExpr <- runExceptT $ parseDocument f content
--                   return $ fromError eContentExpr
-- 
--   app <- ask
-- 
--   let subst :: Map.Map String Expr
--       subst = Map.fromList $ Set.toList (fv mainExpr) `zip` fileExprs
--       rawExpr = apply subst mainExpr
--       l = leftDelim app
--       r = rightDelim app
-- 
--   eitherExprType <- runExceptT $ runTI $ typeInference (Context Map.empty) rawExpr
--   let exprType = fromError eitherExprType
-- 
--   eitherExpr <- runExceptT $ runEv $ reduce rawExpr
--   let expr = fromError eitherExpr
-- 
--   if isTypeQuery app
--   then liftIO $ putStrLn $ show mainExpr ++ " :: " ++ show (generalize (Context Map.empty) exprType)
--   else if isBeingShown app
--        then liftIO $ putStrLn $ show expr ++ " :: " ++ show (generalize (Context Map.empty) exprType)
--        else if not (litsAtTopLevel expr)
--             then error $ "Error: Result has literals in sub expression - `" ++ show expr ++ "` - cannot render soundly."
--             else deepseq exprType $
--                  if outputDest app == Stdout
--                  then liftIO $ LT.putStr $ render (l,r) expr
--                  else liftIO $ LT.writeFile (getFilePath $ outputDest app) $ render (l,r) expr
--   where
--     fromError me = case me of
--       Left err -> error err
--       Right e  -> e


