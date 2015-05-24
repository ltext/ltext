module LText.Parser.Document where

import LText.Internal.Expr

import Text.Parsec
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Except



type Var = String

type HeaderSchema = (String, [Var], String)

showHeader :: HeaderSchema -> String
showHeader (l,vs,r) = unwords $ [l] ++ vs ++ [r]

getHeader :: FilePath -> String -> Except String HeaderSchema
getHeader name line = let line' = words line in
  case line' of
    []                 -> throwE $ "No header declared in `" ++ name ++"`."
    xs | length xs < 2 -> throwE $ "No delimiters declared in the header of `" ++ name ++"`."
       | otherwise -> return ( head line'
                             , init $ drop 1 line'
                             , last line'
                             )


parseDocument :: FilePath -> Parsec LT.Text u Exp
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
    buildExpr :: Either String Exp -> Parsec LT.Text u Exp
    buildExpr (Left body)  = return $ EText [(name, LT.pack body)]
    buildExpr (Right expr) = return expr

    go acc x = buildExpr x >>= (\x' -> return $ EConc x' acc)

    eitherP :: Parsec s u a -> Parsec s u b -> Parsec s u (Either a b)
    eitherP a b = (Left <$> try a) <|> (Right <$> try b)




-- | Parser for expressions. Note - cannot parse @EConc@ or @EText@ constructors -
-- they are implicit, and not considered in evaluation.
parseExpr :: Parsec LT.Text u Exp
parseExpr = parseApp
  where
    parseAbs = do
      char '\\'
      n <- many1 anyChar
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
      n <- many1 anyChar
      return $ EVar n
    parseApp = do
      es <- (parseParen <|> parseAbs <|> parseVar) `sepBy1` space
      return $ foldl1 EApp es


-- | turn head arity to list
makeHeaderSchema :: [String] -> (String, String) -> Exp -> (HeaderSchema, Exp)
makeHeaderSchema vs lr (EAbs n e) = makeHeaderSchema (vs ++ [n]) lr e
makeHeaderSchema vs (l,r) e = ((l,vs,r), e)


render :: (Maybe String, Maybe String) -> Exp -> LT.Text
render (l,r) e
  | hasArity e =
      let l' = fromMaybe (error "No left delimiter for >0 arity result") l
          r' = fromMaybe (error "No right delimiter for >0 arity result") r
          (header, e') = makeHeaderSchema [] (l',r') e
          header' = LT.pack $ showHeader header
          body ex = case ex of
            EText ts    -> LT.unlines $ map snd ts
            EConc e1 e2 -> LT.unlines [body e1, body e2]
      in
      LT.unlines [header', body e']
  | otherwise =
      let body ex = case ex of
            EText ts    -> LT.unlines $ map snd ts
            EConc e1 e2 -> LT.unlines [body e1, body e2]
      in
      body e


-- | Post beta reduction
hasArity :: Exp -> Bool
hasArity (EAbs _ _) = True
hasArity _ = False
