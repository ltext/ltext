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
  lexState <- get
  maybe
    (throwError $ "Parser Error: Empty expression - `" ++ show lexState ++ "`.")
    pure
    (exprSoFar lexState)
parseExpr (TLamb:xs) = do
  lexState <- get
  case lexState of
    (ParseState l f me)
      | l          -> throwError $ "Parser Error: Already in lambda declaration - `" ++ show (TLamb:xs) ++ "`."
      | f && not l -> do -- second condition /should/ be redundant
          put $ lexState { inLambdaDec = True
                         , isFreshScope = False
                         }
          parseExpr xs
      | otherwise -> case me of
          Just e -> throwError $ "Parser broken: lambda after exprSoFar - `" ++ show (TLamb:xs) ++ "`, `" ++ show lexState ++ "`."
          _      -> throwError $ "Parser Error: Lambda declarations must be in fresh expression scope - `" ++ show (TLamb:xs) ++ "`."
parseExpr (TArrow:xs) = do
  lexState <- get
  case lexState of
    (ParseState l f me)
      | not l     -> throwError $ "Parser Error: Not in lambda declaration - `" ++ show (TArrow:xs) ++ "`."
      | f         -> throwError $ "Parser Error: No preceding lambda declaration - `" ++ show (TArrow:xs) ++ "`."
      | isJust me -> throwError $ "Parser broken: arrow after exprSoFar - `" ++ show (TLamb:xs) ++ "`, `" ++ show lexState ++ "`."
      | otherwise -> do
          put $ lexState {inLambdaDec = False, isFreshScope = True}
          parseExpr xs
parseExpr (TIdent n:xs) = do
  lexState <- get
  case lexState of
    (ParseState l f me)
      | l -> do
          e <- parseExpr xs
          return $ EAbs n e
      | f && isNothing me -> do
          put $ lexState { isFreshScope = False
                         , exprSoFar = Just $ EVar n
                         }
          parseExpr xs
      | not f -> case me of
                   Nothing -> throwError $ "Parser broken: identifier not in lambda dec or body - `" ++ show (TIdent n:xs) ++ "`, `" ++ show lexState ++ "`."
                   Just e  -> do
                     put $ lexState {exprSoFar = Just $ EApp e $ EVar n}
                     parseExpr xs
     | otherwise -> throwError $ "Parser broken: identifier not in lambda dec or body - `" ++ show (TIdent n:xs) ++ "`, `" ++ show lexState ++ "`."
parseExpr (TGroup es:xs) = do
  lexState <- get
  case lexState of
    (ParseState l f me)
      | l -> throwError $ "Parser Error: No brackets allowed in lambda declaration - `" ++ show (TGroup es:xs) ++ "`."
      | isNothing me -> do
          e <- parseExpr es
          put $ lexState { exprSoFar = Just e
                         , isFreshScope = False
                         } -- should not be in lambda dec
          parseExpr xs
      | otherwise -> do
          put $ lexState { exprSoFar = Nothing
                         , isFreshScope = True
                         , inLambdaDec = False
                         }
          e <- parseExpr es
          put $ lexState { exprSoFar = Just $ EApp (fromJust me) e
                         , isFreshScope = False
                         , inLambdaDec = False
                         }
          parseExpr xs


makeExpr :: ( Monad m
            , MonadError String m
            ) => String -> m Expr
makeExpr s = do
  ts <- lexer s
  runParse $ parseExpr ts
