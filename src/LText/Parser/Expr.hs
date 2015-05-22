{-# LANGUAGE
    OverloadedStrings
  #-}

module LText.Parser.Expr where

import LText.Internal.Expr
import Data.Attoparsec.Text

import Control.Applicative


-- | Parser for expressions. Note - cannot parse @EConc@ or @EText@ constructors -
-- they are implicit, and not considered in evaluation.
parseExpr :: Parser Exp
parseExpr =
      parseVar
  <|> parseAbs
  <|> parseApp
  where
    parseVar = do
      v <- some letter
      return $ EVar v
    parseAbs = do
      char '\\'
      n <- some letter
      some space
      string "->"
      some space
      e <- parseExpr
      return $ EAbs n e
    parseApp = do
      e1 <- parseExpr
      some space
      e2 <- parseExpr
      return $ EApp e1 e2
