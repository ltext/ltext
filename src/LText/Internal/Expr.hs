{-# LANGUAGE
    TypeSynonymInstances
  , FlexibleInstances
  , FlexibleContexts
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

type ExprVar = String

data Expr = EVar ExprVar
          | EApp Expr Expr
          | EAbs ExprVar Expr
          | ELet ExprVar Expr Expr
          | EText [Span] -- post-concatenation spans of text
          | EConc Expr Expr
  deriving (Eq, Ord)

-- | Recursively checks to see if all @EText@ constructors only occur inside the
-- /first/ @EAbs@ constructor via @EConc@ - if some are in @EApp@, for instance,
-- then you can't successfully @render@ the expression.
litsAtTopLevel :: Expr -> Bool
litsAtTopLevel expr = go expr True
  where
    go :: Expr -> Bool -> Bool
    go (EVar _)     _ = True
    go (EApp e1 e2) _ =
      go e1 False && go e2 False
    go (EAbs _ e) isTopLevel = go e isTopLevel
    go (EText _)  isTopLevel = isTopLevel
    go (EConc e1 e2) isTopLevel =
      isTopLevel && (go e1 isTopLevel && go e2 isTopLevel)

instance Bindable Set.Set ExprVar Expr where
  fv (EVar n)      = Set.singleton n
  fv (EApp e1 e2)  = fv e1 `union` fv e2
  fv (EAbs n e)    = n `Set.delete` fv e
  fv (ELet n x y)  = (n `Set.delete` fv y) `union` fv x
  fv (EText _)     = empty
  fv (EConc e1 e2) = fv e1 `union` fv e2

instance Substitutable Map.Map ExprVar Expr Expr where
  apply s (EVar n)      = fromMaybe (EVar n) $ Map.lookup n s
  apply s (EApp e1 e2)  = EApp (apply s e1) (apply s e2)
  apply s (EAbs n e)    = EAbs n $ apply (n `Map.delete` s) e
  apply s (ELet n x y)  = ELet n (apply s x) $ apply (n `Map.delete` s) y
  apply _ (EText t)     = EText t
  apply s (EConc e1 e2) = EConc (apply s e1) (apply s e2)


instance Show Expr where
   showsPrec _ x = shows (prExp x)

prExp :: Expr -> PP.Doc
prExp (EVar name)     = PP.text name
prExp (ELet x b body) = PP.text "let" PP.<+>
                        PP.text x PP.<+> PP.text "=" PP.<+>
                        prExp b PP.<+> PP.text "in" PP.$$
                        PP.nest 2 (prExp body)
prExp (EApp e1 e2)    = prParenExp e1 PP.<+> prParenExp e2
prExp (EAbs n e)      = PP.char 'λ' PP.<> PP.text n PP.<>
                        PP.char '.' PP.<+> prExp e
prExp (EText fs)      = PP.text "#" PP.<>
                        PP.hcat (PP.punctuate PP.comma $ map (PP.text . fst) fs)
prExp (EConc e1 e2)   = PP.parens (prExp e1) PP.<+> PP.text "<>" PP.<+>
                        PP.parens (prExp e2)

prParenExp :: Expr -> PP.Doc
prParenExp t = case t of
  ELet {}  -> PP.parens (prExp t)
  EApp {}  -> PP.parens (prExp t)
  EAbs {}  -> PP.parens (prExp t)
  EConc {} -> PP.parens (prExp t)
  _        -> prExp t
