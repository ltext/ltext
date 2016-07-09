{-# LANGUAGE
    MultiWayIf
  , FlexibleContexts
  #-}

module LText.Parser.Lexer where

import Control.Monad.State
import Control.Monad.Except


data ExprTokens
  = TLamb
  | TArrow
  | TIdent String
  | TGroup [ExprTokens]
  | TLParen
  | TRParen
  deriving (Eq)

instance Show ExprTokens where
  show TLamb = "\\"
  show TArrow = "->"
  show TLParen = "("
  show TRParen = ")"
  show (TIdent s) = s
  show (TGroup ts) = "{" ++ show ts ++ "}"

data TokenState = TokenState
  { between :: String
  } deriving (Eq, Show)

initTokenState :: TokenState
initTokenState = TokenState ""

-- | Creates a stream of tokens from a list of characters
tokenize :: String -> [ExprTokens]
tokenize s = go $ words s
  where
    go [] = []
    go ("->":xs) = TArrow:go xs
    go ("\\":xs) = TLamb:go xs
    go ("(":xs) = TLParen:go xs
    go (")":xs) = TRParen:go xs
    go (x:xs) = case runState (foldM go' [] x) initTokenState of
      (r, TokenState lastIdent)
        | lastIdent == "" -> r ++ go xs
        | otherwise       -> r ++ [TIdent lastIdent] ++ go xs
      where
        go' :: ( MonadState TokenState m
               ) => [ExprTokens] -> Char -> m [ExprTokens]
        go' acc '\\' = do
          lexState <- get
          put lexState { between = "" }
          return $ acc ++ if between lexState == ""
                          then [TLamb]
                          else [TIdent (between lexState), TLamb]
        go' acc '(' = do
          lexState <- get
          put lexState { between = "" }
          return $ acc ++ if between lexState == ""
                          then [TLParen]
                          else [TIdent (between lexState), TLParen]
        go' acc ')' = do
          lexState <- get
          put lexState { between = "" }
          return $ acc ++ if between lexState == ""
                          then [TRParen]
                          else [TIdent (between lexState), TRParen]
        go' acc x = do
          lexState <- get
          put lexState { between = between lexState ++ [x] }
          return acc

runGroup :: ( Monad m
            , MonadError String m
            ) => StateT Integer m a -> m a
runGroup m = evalStateT m 0

-- | Matches brackets and nests token streams
group :: ( MonadState Integer m
         ) => ([ExprTokens], [ExprTokens])
           -> m ([ExprTokens], [ExprTokens])
group (acc, [])         = pure (acc, [])
group (acc, TLParen:xs) = do
  (layer, xs') <- group ([], xs)
  modify' (+1)
  group (acc ++ [TGroup layer], xs')
group (acc, TRParen:xs) = do
  modify (\x -> x-1)
  return (acc, xs)
group (acc, x:xs) = group (acc ++ [x], xs)


lexer :: ( Monad m
         , MonadError String m
         ) => String -> m [ExprTokens]
lexer s = fst <$> runGroup (group ([], tokenize s))
