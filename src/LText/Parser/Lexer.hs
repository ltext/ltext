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
                | TChar Char
  deriving (Eq)

instance Show ExprTokens where
  show TLamb = "λ"
  show TArrow = "→"
  show TLParen = "("
  show TRParen = ")"
  show (TIdent s) = s
  show (TGroup ts) = "{" ++ show ts ++ "}"


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
      lexState <- get
      if | following lexState == Just FollowsBackslash ->
              throwError $ "Lexer Error: Nested Lambdas - `" ++ show s ++ "`."
         | otherwise -> do
              put $ TokenState $ Just FollowsBackslash
              (:) TLamb <$> go xs
    go ("(":xs) = (:) TLParen <$> go xs
    go (")":xs) = (:) TRParen <$> go xs
    go (x:xs) = let (r, NewTokenState _ lastIdent) = runState (foldM go' [] x) $ NewTokenState Nothing "" in
                if lastIdent == ""
                then (r ++) <$> go xs
                else ((r ++ [TIdent lastIdent]) ++) <$> go xs
      where
        go' :: MonadState NewTokenState m => [ExprTokens] -> Char -> m [ExprTokens]
        go' acc '\\' = do
          lexState <- get
          if followingNew lexState == Just FollowsBackslash
          then error $ "Lexer Error: Nested Lambdas - `" ++ show s ++ "`."
          else do put $ lexState { followingNew = Just FollowsBackslash
                                 , between = "" }
                  if between lexState == ""
                  then return $ acc ++ [TLamb]
                  else return $ acc ++ [TIdent $ between lexState, TLamb]
        go' acc '(' = do lexState <- get
                         put $ lexState { followingNew = Nothing, between = ""}
                         if between lexState == ""
                         then return $ acc ++ [TLParen]
                         else return $ acc ++ [TIdent $ between lexState, TLParen]
        go' acc ')' = do lexState <- get
                         put $ lexState { between = ""}
                         if between lexState == ""
                         then return $ acc ++ [TRParen]
                         else return $ acc ++ [TIdent $ between lexState, TRParen]
        go' acc x = do lexState <- get
                       put lexState {between = between lexState ++ [x]}
                       return acc

data NewTokenState = NewTokenState { followingNew :: Maybe FollowingToken
                                   , between :: String
                                   } deriving (Eq, Show)

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
