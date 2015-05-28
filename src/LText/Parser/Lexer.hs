{-# LANGUAGE
    MultiWayIf
  , FlexibleContexts
  #-}

module LText.Parser.Lexer where

import Control.Monad.State
import Control.Monad.Except


data ExprTokens = TLamb
                | TArrow
                | TLParen
                | TRParen
                | TIdent String
                | TGroup [ExprTokens]
  deriving (Eq)

instance Show ExprTokens where
  show TLamb = "λ"
  show TArrow = "→"
  show TLParen = "("
  show TRParen = ")"
  show (TIdent s) = s
  show (TGroup ts) = "<" ++ show ts ++ ">"


data FollowingToken = FollowsBackslash
  deriving (Show, Eq)

data TokenState = TokenState
  { following :: Maybe FollowingToken
  } deriving (Show, Eq)

initTokenState :: TokenState
initTokenState = TokenState Nothing


runTokens :: ( Monad m
             , MonadError String m
             ) => StateT TokenState m a
               -> m a
runTokens m = evalStateT m initTokenState

tokenize :: ( MonadState TokenState m
            , MonadError String m
            ) => String -> m [ExprTokens]
tokenize s = go $ words s
  where
    go [] = return []
    go ("->":xs) = do
      put $ TokenState Nothing
      (:) TArrow <$> go xs
    go ("\\":xs) = do
      state <- get
      if | following state == Just FollowsBackslash ->
              throwError $ "Lexer Error: Nested Lambdas - `" ++ show s ++ "`."
         | otherwise -> do
              put $ TokenState $ Just FollowsBackslash
              (:) TLamb <$> go xs
    go ("(":xs) = (:) TLParen <$> go xs
    go (")":xs) = (:) TRParen <$> go xs
    go (x:xs) | head x == '\\' = do
                  state <- get
                  if following state == Just FollowsBackslash
                  then throwError $ "Lexer Error: Nested Lambdas - `" ++ show s ++ "`."
                  else do put $ TokenState $ Just FollowsBackslash
                          rest <- go xs
                          return $ TLamb:TIdent (tail x):rest
              | head x == '(' = do rest <- go xs
                                   if last x == ')'
                                   then return $ TLParen:TIdent (tail $ init x):TRParen:rest
                                   else return $ TLParen:TIdent (tail x):rest
              | last x == ')' = do rest <- go xs
                                   return $ TIdent (init x):TRParen:rest
              | otherwise = (:) (TIdent x) <$> go xs


runGroup :: ( Monad m
            , MonadError String m
            ) => StateT Integer m a -> m a
runGroup m = evalStateT m 0

-- | Matches brackets and nests token streams
group :: ( MonadState Integer m
         ) => ([ExprTokens], [ExprTokens])
         -> m ([ExprTokens], [ExprTokens])
group (acc, []) =
  return (acc, [])
group (acc, TLParen:xs) = do
  (layer, rest) <- group ([], xs)
  get >>= \x -> put $ x+1
  group (acc ++ [TGroup layer], rest)
group (acc, TRParen:xs) = do
  get >>= \x -> put $ x-1
  return (acc, xs)
group (acc, x:xs) =
  group (acc ++ [x], xs)


lexer :: ( Monad m
         , MonadError String m
         ) => String -> m [ExprTokens]
lexer s = do
  ts <- runTokens $ tokenize s
  ts' <- runGroup $ group ([],ts)
  return $ fst ts'
