{-# LANGUAGE
    TypeSynonymInstances
  , FlexibleInstances
  , MultiParamTypeClasses
  #-}

module LText.Internal.Inference where

import LText.Internal.Expr
import LText.Internal.Types
import LText.Internal.Classes

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Except
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set


newtype Context = Context (Map.Map ExpVar Prenex)

remove :: Context -> String -> Context
remove (Context env) var = Context (Map.delete var env)


instance Bindable Set.Set TypeVar Context where
  fv (Context env) = fv (Map.elems env)

instance Substitutable Map.Map TypeVar Type Context where
  apply s (Context env) = Context (fmap (apply s) env)


generalize :: Context -> Type -> Prenex
generalize env t = Prenex vars t
  where vars = Set.toList $ fv t `difference` fv env

-- Inference Monad

data TIEnv = TIEnv

data TIState = TIState { tiSupply :: Int
                       , tiSubst  :: Subst
                       }

type TI a = ExceptT String (ReaderT TIEnv (StateT TIState IO)) a

runTI :: TI a -> IO (Either String a, TIState)
runTI t = runStateT (runReaderT (runExceptT t) initTIEnv) initTIState
  where initTIEnv = TIEnv
        initTIState = TIState { tiSupply = 0
                              , tiSubst = nullSubst
                              }

newTyVar :: TypeVar -> TI Type
newTyVar prefix = do
  s <- get
  put s { tiSupply = tiSupply s + 1
        }
  return $ TVar $ prefix ++ show (tiSupply s)

instantiate :: Prenex -> TI Type
instantiate (Prenex vars t) = do
  nvars <- mapM (\ _ -> newTyVar "a") vars
  return $ apply (Map.fromList $ zip vars nvars) t

mgu :: Type -> Type -> TI Subst
mgu (TFun l r) (TFun l' r')  =  do  s1 <- mgu l l'
                                    s2 <- mgu (apply s1 r) (apply s1 r')
                                    return (s1 `composeSubst` s2)
mgu (TVar u) t               =  varBind u t
mgu t (TVar u)               =  varBind u t
mgu TInt TInt                =  return nullSubst
mgu TBool TBool              =  return nullSubst
mgu t1 t2                    =  throwE $ "Substitutable do not unify: " ++ show t1 ++
                                " vs. " ++ show t2

-- | Makes a substitution @[x -> t]@
varBind :: TypeVar -> Type -> TI Subst
varBind u t | t == TVar u         = return nullSubst
            | u `Set.member` fv t = throwE $ "occur check fails: " ++ u ++
                                             " vs. " ++ show t
            | otherwise           = return (Map.singleton u t)

tiLit :: Context -> Lit -> TI (Subst, Type)
tiLit _ (LInt _)   =  return (nullSubst, TInt)
tiLit _ (LBool _)  =  return (nullSubst, TBool)

-- | Type inference function
ti :: Context -> Exp -> TI (Subst, Type)
ti (Context env) (EVar n) = case Map.lookup n env of
  Nothing     ->  throwE $ "unbound variable: " ++ n
  Just sigma  ->  do  t <- instantiate sigma
                      return (nullSubst, t)
ti env (ELit l) = tiLit env l
ti env (EAbs n e) = do
  tv <- newTyVar "a"
  let Context env' = remove env n
      env'' = Context $ env' `Map.union` Map.singleton n (Prenex [] tv)
  (s1, t1) <- ti env'' e
  return (s1, TFun (apply s1 tv) t1)
ti env (EApp e1 e2) = do
  tv <- newTyVar "a"
  (s1, t1) <- ti env e1
  (s2, t2) <- ti (apply s1 env) e2
  s3 <- mgu (apply s2 t1) (TFun t2 tv)
  return (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)
ti env (ELet x e1 e2) = do
  (s1, t1) <- ti env e1
  let Context env' = remove env x
      t' = generalize (apply s1 env) t1
      env'' = Context (Map.insert x t' env')
  (s2, t2) <- ti (apply s1 env'') e2
  return (s1 `composeSubst` s2, t2)

typeInference :: Context -> Exp -> TI Type
typeInference env e = do
  (s, t) <- ti env e
  return (apply s t)
