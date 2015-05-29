{-# LANGUAGE
    FlexibleContexts
  , MultiWayIf
  #-}

module LText.Parser.Expr where

import LText.Parser.Lexer
import LText.Internal.Expr

import Data.Maybe
import Control.Monad.State
import Control.Monad.Except


data ParseState = ParseState
  { inLambdaDec :: Bool -- no groups allowed
  , isFreshScope :: Bool -- lambda decs need to be fresh
  , exprSoFar :: Maybe Expr -- facilitates left associativity
  } deriving (Show, Eq)

initParseState :: ParseState
initParseState = ParseState False True Nothing

runParse :: ( Monad m
            , MonadError String m
            ) => StateT ParseState m a
              -> m a
runParse m = evalStateT m initParseState


-- | Parser for expressions. Note - cannot parse @EConc@ or @EText@ constructors -
-- they are implicit, and not considered in evaluation.
parseExpr :: ( MonadState ParseState m
             , MonadError String m
             ) => [ExprTokens] -> m Expr
parseExpr [] = do
  state <- get
  if | isNothing (exprSoFar state) -> throwError $ "Parser Error: Empty Sub-expression - `" ++ show state ++ "`."
     | otherwise -> return $ fromJust $ exprSoFar state
parseExpr (TLamb:xs) = do
  state <- get
  if | inLambdaDec state -> throwError $ "Parser Error: Already in lambda declaration - `" ++ show (TLamb:xs) ++ "`."
     | isFreshScope state && not (inLambdaDec state) -> do -- second condition /should/ be redundant
          put $ state {inLambdaDec = True, isFreshScope = False}
          parseExpr xs
     | isJust (exprSoFar state) -> throwError $ "Parser broken: lambda after exprSoFar - `" ++ show (TLamb:xs) ++ "`, `" ++ show state ++ "`."
     | otherwise -> throwError $ "Parser Error: Lambda declarations must be in fresh expression scope - `" ++ show (TLamb:xs) ++ "`."
parseExpr (TArrow:xs) = do
  state <- get
  if | not (inLambdaDec state) -> throwError $ "Parser Error: Not in lambda declaration - `" ++ show (TArrow:xs) ++ "`."
     | isFreshScope state -> throwError $ "Parser Error: No preceding lambda declaration - `" ++ show (TArrow:xs) ++ "`."
     | isJust (exprSoFar state) -> throwError $ "Parser broken: arrow after exprSoFar - `" ++ show (TLamb:xs) ++ "`, `" ++ show state ++ "`."
     | otherwise -> do
          put $ state {inLambdaDec = False, isFreshScope = True}
          parseExpr xs
parseExpr (TIdent n:xs) = do
  state <- get
  if | inLambdaDec state -> do
          e <- parseExpr xs
          return $ EAbs n e
     | isFreshScope state
       && isNothing (exprSoFar state) -> do
          put $ state { isFreshScope = False
                      , exprSoFar = Just $ EVar n
                      }
          parseExpr xs
     | not (isFreshScope state)
       && isJust (exprSoFar state) -> do
          put $ state {exprSoFar = Just $ EApp (fromJust $ exprSoFar state) $ EVar n}
          parseExpr xs
     | otherwise -> throwError $ "Parser broken: identifier not in lambda dec or body - `" ++ show (TIdent n:xs) ++ "`, `" ++ show state ++ "`."
parseExpr (TGroup es:xs) = do
  state <- get
  if | inLambdaDec state -> throwError $ "Parser Error: No brackets allowed in lambda declaration - `" ++ show (TGroup es:xs) ++ "`."
     | isNothing (exprSoFar state) -> do
          e <- parseExpr es
          put $ state { exprSoFar = Just e
                      , isFreshScope = False } -- should not be in lambda dec
          parseExpr xs
     | otherwise -> do
          let prev = exprSoFar state
          put $ state { exprSoFar = Nothing
                      , isFreshScope = True
                      , inLambdaDec = False }
          e <- parseExpr es
          state' <- get
          put $ state { exprSoFar = Just $ EApp (fromJust prev) e
                      , isFreshScope = False
                      , inLambdaDec = False }
          parseExpr xs


makeExpr :: ( Monad m
            , MonadError String m
            ) => String -> m Expr
makeExpr s = do
  ts <- lexer s
  runParse $ parseExpr ts


testParse s = do
  eitherExpr <- runExceptT $ makeExpr s
  case eitherExpr of
    Left err -> error err
    Right expr -> return expr
