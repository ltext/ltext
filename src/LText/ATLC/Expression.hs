{-# LANGUAGE OverloadedStrings #-}

module LText.ATLC.Expression where

import LText.ATLC.Types
import Data.Attoparsec.Text
import qualified Data.Text as T

data ATLCSym = ATLCName String
             | ATLCAbst String
             | ATLCApplInner
             | ATLCApplOuter
             | ATLCAppend
             | ATLCConAppend
             | ATLCLit String
  deriving (Show, Eq)

lexExpr :: Parser ATLCSym
lexExpr = lexAbst
  where
  lexAbst = do
    char '\\'
    -- scan


parseExpr :: String -> Either String Expr
parseExpr
