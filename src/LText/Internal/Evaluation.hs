{-# LANGUAGE
    FlexibleContexts
  #-}

module LText.Internal.Evaluation where

import LText.Internal.Expr
import LText.Internal.Classes

import qualified Data.Map as Map

import Control.Monad.Except
import Control.Monad.State
import Control.Applicative ((<$>))


runEv :: ( Monad m
         , MonadError String m
         ) => StateT (Int, Bool) m a -> m a
runEv e = evalStateT e (0, True)

freshExprVar :: ( Monad m
               , MonadState (Int, Bool) m
               ) => String -> m String
freshExprVar prefix = do
  (n,fresh) <- get
  put (n+1, fresh)
  return $ prefix ++ show n


reduce :: ( Monad m
          , Functor m
          , MonadState (Int, Bool) m
          ) => Expr -> m Expr
reduce (EVar n)     = return $ EVar n
reduce (EApp e1 e2) = do
  (n,fresh) <- get
  put (n,True)
  e1' <- reduce e1
  case e1' of
    EAbs n e3 -> do normRight <- alpha =<< reduce e2
                    reduce $ apply (Map.singleton n normRight) e3 -- beta . alpha
    _ -> return $ EApp e1' e2
reduce (EAbs n e1) = do
  (d,fresh) <- get
  if fresh
  then case e1 of
    EApp e2 (EVar m) | n == m -> reduce e2                        -- eta
                     | otherwise -> do put (d,False)
                                       e2' <- reduce e2
                                       return $ EAbs n (EApp e2' $ EVar m)
    _ -> do put (d,False)
            EAbs n <$> reduce e1
  else EAbs n <$> reduce e1
reduce (ELet n x y)  = do normRight <- alpha =<< reduce x
                          reduce $ apply (Map.singleton n normRight) y
reduce (EText fs)    = return $ EText fs
reduce (EConc e1 e2) = liftM2 EConc (reduce e1) (reduce e2)

alpha :: ( Monad m
         , MonadState (Int, Bool) m
         ) => Expr -> m Expr
alpha = go []
  where
    go xs (EAbs n e1)
      | n `elem` xs = do n' <- freshExprVar n
                         e1' <- go (n:xs) e1
                         return $ EAbs n' $ apply (Map.singleton n $ EVar n') e1'
      | otherwise = (return . EAbs n) =<< go (n:xs) e1
    go xs (EApp e1 e2) = liftM2 EApp (go xs e1) (go xs e2)
    go _  (EVar n) = return $ EVar n
    go _  (EText ts) = return $ EText ts
    go xs (EConc e1 e2) = liftM2 EConc (go xs e1) (go xs e2)
