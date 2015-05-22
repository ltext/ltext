{-# LANGUAGE
    TypeSynonymInstances
  , FlexibleInstances
  , MultiParamTypeClasses
  #-}

module LText.Internal.Types where

import LText.Internal.Classes

import           Data.Maybe
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import qualified Text.PrettyPrint           as PP


type TypeVar = String

data Type = TVar TypeVar
          | TFun Type Type
          | TText
  deriving (Eq, Ord)

data Prenex = Prenex [TypeVar] Type

type Subst name domain = Map.Map name domain


nullSubst :: Subst TypeVar Type
nullSubst = Map.empty

composeSubst :: Subst TypeVar Type
             -> Subst TypeVar Type
             -> Subst TypeVar Type
composeSubst s1 s2 = fmap (apply s1) s2 `union` s1


instance Bindable Set.Set TypeVar Type where
  fv (TVar n)     = Set.singleton n
  fv (TFun t1 t2) = fv t1 `union` fv t2
  fv TText        = empty

instance Substitutable Map.Map TypeVar Type Type where
  apply s (TVar n)     = fromMaybe (TVar n) $ Map.lookup n s
  apply s (TFun t1 t2) = TFun (apply s t1) (apply s t2)
  apply _ t            = t


instance Bindable Set.Set TypeVar Prenex where
  fv (Prenex vars t) = fv t `difference` Set.fromList vars

instance Substitutable Map.Map TypeVar Type Prenex where
  apply s (Prenex vars t) = Prenex vars $ apply (foldr Map.delete s vars) t


instance Show Type where
  showsPrec _ x = shows (prType x)

prType :: Type -> PP.Doc
prType (TVar n)   = PP.text n
prType (TFun t s) = prParenType t PP.<+> PP.text "->" PP.<+> prType s
prType TText      = PP.text "Text"

prParenType :: Type -> PP.Doc
prParenType t = case t of
  TFun _ _  -> PP.parens (prType t)
  _         -> prType t

instance Show Prenex where
  showsPrec _ x = shows (prPrenex x)

prPrenex :: Prenex -> PP.Doc
prPrenex (Prenex vars t) = PP.text "∀" PP.<+>
                           PP.hcat (PP.punctuate PP.comma (map PP.text vars))
                           PP.<> PP.text "." PP.<+> prType t
