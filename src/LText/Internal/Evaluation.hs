{-# LANGUAGE
    FlexibleContexts
  #-}

module LText.Internal.Evaluation where

import LText.Internal.Expr
import LText.Internal.Classes

import qualified Data.Set as Set
import qualified Data.Map as Map


beta :: (Substitutable Map.Map ExpVar Exp Exp
        ) =>
        Exp -> Exp
beta (EVar n)     = EVar n
beta (EApp e1 e2) = case beta e1 of
  EAbs n e1' -> apply (Map.singleton n $ beta e2) e1'
  _          -> error "can't apply that sheeit bitch yarredde kno"
beta (EAbs n e)   = EAbs n $ beta e
beta (ELet n x y) = apply (Map.singleton n $ beta x) y
