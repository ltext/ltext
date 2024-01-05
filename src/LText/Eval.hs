{-# LANGUAGE
    FlexibleContexts
  #-}

module LText.Eval where

import LText.Expr (Expr (..))

import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as HS


evaluate :: Expr -> Expr
evaluate e =
  case e of
    Abs n e' -> Abs n (evaluate e')
    Concat e1 e2 s err ->
      case (evaluate e1, evaluate e2) of
        (Lit t1 _ _, Lit t2 _ _) -> Lit (t1 ++ t2) s err -- forget children?
        (e1'   , e2'   ) -> Concat e1' e2' s err
    App e1 e2 ->
      case evaluate e1 of
        Abs n e1' -> substitute n (evaluate e2) (evaluate e1')
        e1'       -> App e1' (evaluate e2)
    _ -> e


substitute :: String -> Expr -> Expr -> Expr
substitute n x e =
  case e of
    Concat e1 e2 s err    -> Concat (substitute n x e1) (substitute n x e2) s err
    Var n'    | n == n'   -> x
              | otherwise -> Var n'
    App e1 e2             -> App (substitute n x e1) (substitute n x e2)
    Abs n' e' | n == n'   -> Abs n' e'
              | otherwise -> Abs n' $ substitute n x e'
    _ -> e


freeVars :: Expr -> HashSet String
freeVars e =
  case e of
    Abs n e'     -> HS.delete n $ freeVars e'
    App e1 e2    -> freeVars e1 <> freeVars e2
    Var n        -> HS.singleton n
    Lit _ _ _    -> HS.empty
    Concat e1 e2 _ _ -> freeVars e1 <> freeVars e2

