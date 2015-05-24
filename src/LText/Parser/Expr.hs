{-# LANGUAGE
    OverloadedStrings
  #-}

module LText.Parser.Expr where

import LText.Internal.Expr
import Text.Parsec
import qualified Data.Text as T



-- | Parser for expressions. Note - cannot parse @EConc@ or @EText@ constructors -
-- they are implicit, and not considered in evaluation.
parseExpr :: Parsec T.Text u Exp
parseExpr = parseApp
  where
    parseAbs = do
      char '\\'
      n <- many1 letter
      skipMany space
      string "->"
      skipMany space
      e <- parseExpr
      return $ EAbs n e
    parseParen = do
      char '('
      skipMany space
      e <- parseExpr
      skipMany space
      char ')'
      return e
    parseVar = do
      n <- many1 letter
      return $ EVar n
    parseApp = do
      es <- (parseParen <|> parseAbs <|> parseVar) `sepBy1` space
      return $ foldl1 EApp es
