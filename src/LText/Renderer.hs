module LText.Renderer where

import LText.Internal.Expr

import qualified Data.Text.Lazy as LT
import Data.Maybe


type Var = String
type HeaderSchema = (String, [Var], String)


showHeader :: HeaderSchema -> String
showHeader (l,vs,r) = unwords $ [l] ++ vs ++ [r]


-- | turn head of template's arity into list - still need to render inner expressions
renderHeaderSchema :: [String] -> (String, String) -> Exp -> (HeaderSchema, Exp)
renderHeaderSchema vs lr (EAbs n e) = renderHeaderSchema (vs ++ [n]) lr e
renderHeaderSchema vs (l,r) e       = ((l,vs,r), e)


render :: (Maybe String, Maybe String) -> Exp -> LT.Text
render (l,r) e
  | hasArity e =
      let l' = fromMaybe (error "Error: No left delimiter supplied - result has arity `> 0`.") l
          r' = fromMaybe (error "Error: No right delimiter supplied - result has arity `> 0`.") r
          (header, e') = renderHeaderSchema [] (l',r') e
          header' = LT.pack $ showHeader header
      in
      LT.unlines [header', renderBody e']
  | otherwise = renderBody e
  where
    renderBody (EText ts) = LT.unlines $ concatMap (LT.lines . snd) ts
    renderBody (EConc e1 e2) = LT.unlines [renderBody e1, renderBody e2]
    renderBody expr = case (l,r) of
      (Just l', Just r') -> LT.pack $ l' ++ " " ++ show expr ++ " " ++ r'
      _ -> error "Error: free variables occurred during rendering."


-- | Note - only use post-beta reduction: this function is partial
hasArity :: Exp -> Bool
hasArity (EAbs _ _) = True
hasArity _          = False
