module LText.Internal.Expr.TH where

import LText.Internal.Expr
import qualified Data.Text.Lazy as LT
import Language.Haskell.TH


exprToHs :: Expr -> Q Exp
exprToHs (EVar n) =
  return $ VarE $ mkName n
exprToHs (EApp e1 e2) = do
  e1' <- exprToHs e1
  e2' <- exprToHs e2
  return $ AppE e1' e2'
exprToHs (EAbs n e) = do
  e' <- exprToHs e
  return $ LamE [VarP $ mkName n] e'
exprToHs (EText ts) = let content = LT.unpack $ LT.unlines $ map snd ts in
  return $ LitE $ StringL content
exprToHs (EConc e1 e2) = do
  e1' <- exprToHs e1
  e2' <- exprToHs e2
  return $ AppE (AppE (ParensE (VarE $ mkName "++")) e1') e2'
