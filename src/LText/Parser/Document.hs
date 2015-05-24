module LText.Parser.Document where

import LText.Parser.Header
import LText.Parser.Expr
import LText.Internal.Expr

import Text.Parsec
import qualified Data.Text as T

import Data.Composition
import Control.Monad.Except


parseDocument :: FilePath -> Parsec T.Text u Exp
parseDocument name = do
  firstLine <- manyTill anyChar newline
  let (l,vs,r) = case runExcept $ getHeader name firstLine of
                      Right hs -> hs
                      Left err -> error err
  chunks <- many $ eitherP (manyTill anyChar $ string l)
                           (string l *> (parseExpr <* string r))
  lastExpr <- buildExpr $ head chunks
  bodyExpr <- foldM go lastExpr $ tail chunks
  foldM (\acc n -> return $ EAbs n acc) bodyExpr vs
  where
    buildExpr :: Either String Exp -> Parsec T.Text u Exp
    buildExpr (Left body)  = return $ EText [(name, T.pack body)]
    buildExpr (Right expr) = return expr

    go acc x = buildExpr x >>= (\x' -> return $ EConc x' acc)

    eitherP :: Parsec s u a -> Parsec s u b -> Parsec s u (Either a b)
    eitherP a b = (Left <$> try a) <|> (Right <$> try b)
