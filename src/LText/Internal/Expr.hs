module LText.Internal.Expr where

import qualified Text.PrettyPrint as PP


data Exp = EVar ExpVar
         | ELit Lit
         | EApp Exp Exp
         | EAbs ExpVar Exp
         | ELet ExpVar Exp Exp
  deriving (Eq, Ord)

type ExpVar = String

data Lit = LInt Integer
         | LBool Bool
  deriving (Eq, Ord)

instance Show Exp where
   showsPrec _ x = shows (prExp x)

prExp :: Exp -> PP.Doc
prExp (EVar name)     = PP.text name
prExp (ELit lit)      = prLit lit
prExp (ELet x b body) = PP.text "let" PP.<+>
                        PP.text x PP.<+> PP.text "=" PP.<+>
                        prExp b PP.<+> PP.text "in" PP.$$
                        PP.nest 2 (prExp body)
prExp (EApp e1 e2)    = prExp e1 PP.<+> prParenExp e2
prExp (EAbs n e)      = PP.char '\\' PP.<+> PP.text n PP.<+>
                        PP.text "->" PP.<+>
                        prExp e

prParenExp :: Exp -> PP.Doc
prParenExp t = case t of
  ELet {} -> PP.parens (prExp t)
  EApp {} -> PP.parens (prExp t)
  EAbs {} -> PP.parens (prExp t)
  _       -> prExp t

instance Show Lit where
  showsPrec _ x = shows (prLit x)

prLit :: Lit -> PP.Doc
prLit (LInt i)  = PP.integer i
prLit (LBool b) = PP.text $ show b
