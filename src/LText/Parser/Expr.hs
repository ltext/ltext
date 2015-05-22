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
parseExpr = parseApp
  where
    parseAbs = do
      char '\\'
      n <- some letter
      skipSpace
      string "->"
      skipSpace
      e <- parseExpr
      return $ EAbs n e
    parseParen = do
      char '('
      skipSpace
      e <- parseExpr
      skipSpace
      char ')'
      return e
    parseVar = do
      n <- some letter
      return $ EVar n
    parseApp = do
      es <- (parseParen <|> parseAbs <|> parseVar) `sepBy1` space
      return $ foldl1 EApp es
