{-# LANGUAGE
    ScopedTypeVariables
  , FlexibleContexts
  #-}

module LText.Parser.Document where

import LText.Internal.Expr

import Text.Parsec
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import Debug.Trace (traceShow)
import Control.DeepSeq (force)



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

parseBody :: Monad m => String -> ParsecT LT.Text u m String
parseBody l = manyTill anyChar $ try $ eof <|> (return () <* string l)

parseDelim :: Monad m => (String, String) -> ParsecT LT.Text u m Exp
parseDelim (l,r) = between (string l) (string r) parseExpr

parseChunks :: Monad m => (String, String) -> ParsecT LT.Text u m [Either String Exp]
parseChunks (l,r) = many $ eitherP (parseBody l) $ parseDelim (l,r)

parseDocument :: MonadIO m =>
                 FilePath -> ParsecT LT.Text u m Exp
parseDocument name = do
  firstLine <- manyTill anyChar newline
  let (l,vs,r) = case runExcept $ getHeader name firstLine of
                      Right hs -> hs
                      Left err -> error err
  -- top-first, bottom-last
  (chunks :: [Either String Exp]) <- parseChunks (l,r)

  liftIO $ print chunks
  bodyExprFirst <- buildExpr $ head chunks
  bodyExpr <- foldM go bodyExprFirst $ tail chunks
  foldM (\acc n -> return $ EAbs n acc) bodyExpr vs
  where
    buildExpr :: Monad m =>
                 Either String Exp -> ParsecT LT.Text u m Exp
    buildExpr (Left body)  = return $ EText [(name, LT.pack body)]
    buildExpr (Right expr) = return expr

    go acc = liftM (EConc acc) . buildExpr -- right-append to EConc


eitherP :: Monad m =>
           ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m (Either a b)
eitherP a b = (Left <$> a) <|> (Right <$> b)



-- | Parser for expressions. Note - cannot parse @EConc@ or @EText@ constructors -
-- they are implicit, and not considered in evaluation.
parseExpr :: Monad m =>
             ParsecT LT.Text u m Exp
parseExpr = parseApp
  where
    parseAbs = do
      char '\\'
      n <- many1 (alphaNum <|> char '.')
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
      n <- many1 (alphaNum <|> char '.')
      return $ EVar n
    parseApp = do
      es <- (parseParen <|> parseAbs <|> parseVar) `sepBy1` space
      return $ foldl1 EApp es


-- | turn head of template's arity into list - still need to render inner expressions
renderHeaderSchema :: [String] -> (String, String) -> Exp -> (HeaderSchema, Exp)
renderHeaderSchema vs lr (EAbs n e) = renderHeaderSchema (vs ++ [n]) lr e
renderHeaderSchema vs (l,r) e       = ((l,vs,r), e)


render :: (Maybe String, Maybe String) -> Exp -> LT.Text
render (l,r) e
  | hasArity e =
      let l' = fromMaybe (error "No left delimiter supplied for result with > 0 arity") l
          r' = fromMaybe (error "No right delimiter supplied for result with > 0 arity") r
          (header, e') = renderHeaderSchema [] (l',r') e
          header' = LT.pack $ showHeader header
      in
      LT.unlines [header', renderBody e']
  | otherwise = renderBody e
  where
    renderBody (EText ts) = LT.unlines $ concatMap (LT.lines . snd) ts
    renderBody (EConc e1 e2) = LT.unlines [renderBody e1, renderBody e2]


-- | Note - only use post-beta reduction: this function is partial
hasArity :: Exp -> Bool
hasArity (EAbs _ _) = True
hasArity _          = False
