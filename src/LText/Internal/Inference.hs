{-# LANGUAGE
    TypeSynonymInstances
  , FlexibleInstances
  , MultiParamTypeClasses
  , FlexibleContexts
  #-}

module LText.Internal.Inference where

import LText.Internal.Expr
import LText.Internal.Types
import LText.Internal.Classes

import           Control.Monad.State
import           Control.Monad.Except
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set


newtype Context = Context (Map.Map ExprVar Prenex)

remove :: Context -> String -> Context
remove (Context env) var = Context (Map.delete var env)


instance Bindable Set.Set TypeVar Context where
  fv (Context env) = fv (Map.elems env)

instance Substitutable Map.Map TypeVar Type Context where
  apply s (Context env) = Context (fmap (apply s) env)


-- | Binds free type variables as universally quantified
generalize :: Context -> Type -> Prenex
generalize env t = Prenex vars t
  where vars = Set.toList $ fv t `difference` fv env

data TIState = TIState { tiSupply :: Int
                       , tiSubst  :: Subst TypeVar Type
                       }


runTI :: ( Monad m
         , MonadError String m
         ) => StateT TIState m a -> m a
runTI t = evalStateT t initTIState
  where initTIState = TIState { tiSupply = 0
                              , tiSubst = nullSubst
                              }


newTyVar :: ( Monad m
            , MonadState TIState m
            , MonadError String m
            ) => TypeVar -> m Type
newTyVar prefix = do
  s <- get
  put s {tiSupply = tiSupply s + 1}
  return $ TVar $ prefix ++ show (tiSupply s)

-- | Replaces bound type variables with free, fresh ones
instantiate :: ( Monad m
               , MonadState TIState m
               , MonadError String m
               ) => Prenex -> m Type
instantiate (Prenex vars t) = do
  nvars <- mapM newTyVar vars
  return $ apply (Map.fromList $ zip vars nvars) t

-- | Most general unifier
mgu :: ( Monad m
       , MonadState TIState m
       , MonadError String m
       ) => Type -> Type -> m (Subst TypeVar Type)
mgu (TFun l r) (TFun l' r')  = do s1 <- mgu l l'
                                  s2 <- mgu (apply s1 r) (apply s1 r')
                                  return (s1 `composeSubst` s2)
mgu (TVar u) t               = varBind u t
mgu t (TVar u)               = varBind u t
mgu TText TText              = return nullSubst
mgu t1 t2                    = throwError $ "Types do not unify: " ++ show t1 ++
                                            " vs. " ++ show t2

-- | Makes a substitution @[x -> t]@
varBind :: ( Monad m
           , MonadState TIState m
           , MonadError String m
           ) => TypeVar -> Type -> m (Subst TypeVar Type)
varBind u t | t == TVar u         = return nullSubst
            | u `Set.member` fv t = throwError $ "Occur check fails: " ++ u ++
                                                 " vs. " ++ show t
            | otherwise           = return (Map.singleton u t)


-- | Type inference function
ti :: ( Monad m
      , MonadState TIState m
      , MonadError String m
      ) => Context -> Expr -> m (Subst TypeVar Type, Type)
ti (Context env) (EVar n) = case Map.lookup n env of
  Nothing    -> throwError $ "Unbound variable: " ++ n
  Just sigma -> do t <- instantiate sigma
                   return (nullSubst, t)

ti env (EAbs n e) = do
  tv <- newTyVar "a"
  let Context env' = remove env n -- replace `n`'s type with a free type variable
      env''        = Context $ env' `Map.union` Map.singleton n (Prenex [] tv)
  (s1, t1) <- ti env'' e
  return (s1, TFun (apply s1 tv) t1)

ti env (EApp e1 e2) = do
  (s1, t1) <- ti env e1
  (s2, t2) <- ti (apply s1 env) e2
  tv       <- newTyVar "a"
  s3       <- mgu (apply s2 t1) (TFun t2 tv)
  return (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)

ti env (ELet x e1 e2) = do
  (s1, t1) <- ti env e1
  let Context env' = remove env x
      t'           = generalize (apply s1 env) t1
      env''        = Context (Map.insert x t' env')
  (s2, t2) <- ti (apply s1 env'') e2
  return (s1 `composeSubst` s2, t2)

ti _ (EText _) = return (nullSubst, TText)

ti env (EConc e1 e2) = do
  (s1, t1) <- ti env e1
  (s2, t2) <- ti env e2
  s3 <- mgu t1 t2
  s4 <- mgu (apply s3 t1) TText
  return (s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1, TText)


typeInference :: ( Monad m
                 , MonadState TIState m
                 , MonadError String m
                 ) => Context -> Expr -> m Type
typeInference env e = do
  (s, t) <- ti env e
  return (apply s t)


test :: ( Monad m
        , MonadIO m
        ) => Expr -> m ()
test e = do
  eRes <- runExceptT $ runTI $ typeInference (Context Map.empty) e
  case eRes of
    Left err -> liftIO $ putStrLn $ "Error: " ++ err
    Right t  -> let q = generalize (Context Map.empty) t in
      liftIO $ putStrLn $ show e ++ " :: " ++ show q
