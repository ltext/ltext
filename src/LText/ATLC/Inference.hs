module LText.ATLC.Inference where

import LText.ATLC.Types

canonicalTypeOf :: Expr -> TypeSig
canonicalTypeOf Literal = TypeLit Z
canonicalTypeOf (RExpr (RTerm s)) = TypeTerm [s] []
canonicalTypeOf (RExpr (Apply x y)) = canonicalTypeOf x 
