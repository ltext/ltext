{-# LANGUAGE
    TypeSynonymInstances
  , FlexibleInstances
  , MultiParamTypeClasses
  #-}

module LText.Internal.Expr where

import LText.Internal.Classes

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Text.PrettyPrint as PP
import qualified Data.Text.Lazy as LT

import Data.Maybe



type Span = (FilePath, LT.Text)

type ExpVar = String

data Exp = EVar ExpVar
         | EApp Exp Exp
         | EAbs ExpVar Exp
         | ELet ExpVar Exp Exp
         | EText [Span] -- post-concatenation spans of text
         | EConc Exp Exp
  deriving (Eq, Ord)

instance Bindable Set.Set ExpVar Exp where
  fv (EVar n)      = Set.singleton n
  fv (EApp e1 e2)  = fv e1 `union` fv e2
  fv (EAbs n e)    = n `Set.delete` fv e
  fv (ELet n x y)  = (n `Set.delete` fv y) `union` fv x
  fv (EText _)     = empty
  fv (EConc e1 e2) = fv e1 `union` fv e2

instance Substitutable Map.Map ExpVar Exp Exp where
  apply s (EVar n)      = fromMaybe (EVar n) $ Map.lookup n s
  apply s (EApp e1 e2)  = EApp (apply s e1) (apply s e2)
  apply s (EAbs n e)    = EAbs n $ apply (n `Map.delete` s) e
  apply s (ELet n x y)  = ELet n (apply s x) $ apply (n `Map.delete` s) y
  apply _ (EText t)     = EText t
  apply s (EConc e1 e2) = EConc (apply s e1) (apply s e2)


instance Show Exp where
   showsPrec _ x = shows (prExp x)

prExp :: Exp -> PP.Doc
prExp (EVar name)     = PP.text name
prExp (ELet x b body) = PP.text "let" PP.<+>
                        PP.text x PP.<+> PP.text "=" PP.<+>
                        prExp b PP.<+> PP.text "in" PP.$$
                        PP.nest 2 (prExp body)
prExp (EApp e1 e2)    = PP.parens (prExp e1) PP.<+> prParenExp e2
prExp (EAbs n e)      = PP.char 'λ' PP.<> PP.text n PP.<>
                        PP.char '.' PP.<+> prExp e
prExp (EText fs)      = PP.text "#" PP.<>
                        PP.hcat (PP.punctuate PP.comma $ map (PP.text . fst) fs)
prExp (EConc e1 e2)   = PP.parens (prExp e1) PP.<+> PP.text "<>" PP.<+>
                        PP.parens (prExp e2)

prParenExp :: Exp -> PP.Doc
prParenExp t = case t of
  ELet {}  -> PP.parens (prExp t)
  EApp {}  -> PP.parens (prExp t)
  EAbs {}  -> PP.parens (prExp t)
  EConc {} -> PP.parens (prExp t)
  _        -> prExp t
