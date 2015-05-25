{-# LANGUAGE
    FlexibleContexts
  #-}

module LText.Internal.Evaluation where

import LText.Internal.Expr
import LText.Internal.Classes

import qualified Data.Set as Set
import qualified Data.Map as Map

import Data.Composition
import Control.Monad.Composition
import Control.Monad.Trans.Except
import Control.Monad.State


type EV a = ExceptT String (StateT Int IO) a

runEv :: EV a -> IO a
runEv e = (evalStateT $ runExceptT e) 0 >>= \x -> case x of
  Left err -> error err
  Right e' -> return e'

freshExpVar :: String -> EV String
freshExpVar prefix = do
  s <- get
  put $ s + 1
  return $ prefix ++ show s


-- reduce :: (Substitutable Map.Map ExpVar Exp Exp
--           ) =>
--           Exp -> EV Exp
reduce (EVar n)      = return $ EVar n
reduce (EApp e1 e2)  = do
  e1' <- reduce e1
  case e1' of
    EAbs n e1' -> do normRight <- alpha =<< reduce e2
                     reduce $ apply (Map.singleton n normRight) e1' -- beta . alpha
    _ -> return $ EApp e1' e2
reduce (EAbs n e1) = do
  e1' <- reduce e1
  case e1' of
    EApp e2 (EVar m) | n == m -> return e2 -- eta
                     | otherwise -> return $ EAbs n e1'
    _ -> return $ EAbs n e1'
reduce (ELet n x y) = do
  normRight <- alpha =<< reduce x
  reduce $ apply (Map.singleton n normRight) y
reduce (EText fs)    = return $ EText fs
reduce (EConc e1 e2) = (return .* EConc) ==<< reduce e1 =<< reduce e2


-- alpha :: Exp -> EV Exp
alpha e = go [] e
  where
    go xs (EAbs n e1)
      | n `elem` xs = do n' <- freshExpVar n
                         e1' <- go (n:xs) e1
                         return $ EAbs n' $ apply (Map.singleton n $ EVar n') e1'
      | otherwise = (return . EAbs n) =<< go (n:xs) e1
    go xs (EApp e1 e2) = (return .* EApp) ==<< go xs e1 =<< go xs e2
    go xs (EVar n) = return $ EVar n
