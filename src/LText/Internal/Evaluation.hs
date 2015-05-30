{-# LANGUAGE
    FlexibleContexts
  #-}

module LText.Internal.Evaluation where

import LText.Internal.Expr
import LText.Internal.Classes
import LText.Internal.Inference
import LText.Internal.Types

import qualified Data.Map as Map

import Data.Composition
import Control.Monad.Composition
import Control.Monad.Except
import Control.Monad.State


runEv :: ( Monad m
         , MonadError String m
         ) => StateT Int m a -> m a
runEv e = evalStateT e 0

freshExprVar :: ( Monad m
               , MonadState Int m
               ) => String -> m String
freshExprVar prefix = do
  s <- get
  put $ s + 1
  return $ prefix ++ show s


reduce :: ( Monad m
          , MonadState Int m
          ) => Expr -> m Expr
reduce (EVar n)      = return $ EVar n
reduce (EApp e1 e2)  = do
  e1' <- reduce e1
  case e1' of
    EAbs n e1' -> do normRight <- alpha =<< reduce e2
                     reduce $ apply (Map.singleton n normRight) e1' -- beta . alpha
    _ -> return $ EApp e1' e2
reduce (EAbs n e1) =
  case e1 of
    EApp e2 (EVar m) | n == m -> reduce e2 -- eta
                     | otherwise -> do e2' <- reduce e2
                                       return $ EAbs n (EApp e2' $ EVar m)
    _ -> EAbs n <$> reduce e1
reduce (ELet n x y) = do
  normRight <- alpha =<< reduce x
  reduce $ apply (Map.singleton n normRight) y
reduce (EText fs)    = return $ EText fs
reduce (EConc e1 e2) = liftM2 EConc (reduce e1) (reduce e2)
-- concatenation isn't packin'

alpha :: ( Monad m
         , MonadState Int m
         ) => Expr -> m Expr
alpha = go []
  where
    go xs (EAbs n e1)
      | n `elem` xs = do n' <- freshExprVar n
                         e1' <- go (n:xs) e1
                         return $ EAbs n' $ apply (Map.singleton n $ EVar n') e1'
      | otherwise = (return . EAbs n) =<< go (n:xs) e1
    go xs (EApp e1 e2) = (return .* EApp) ==<< go xs e1 =<< go xs e2
    go _  (EVar n) = return $ EVar n
    go _  (EText ts) = return $ EText ts
    go xs (EConc e1 e2) = (return .* EConc) ==<< go xs e1 =<< go xs e2
