{-# LANGUAGE
    ScopedTypeVariables
  , FlexibleContexts
  , MultiWayIf
  #-}

module LText.Parser.Document where

import LText.Parser.Lexer
import LText.Internal.Expr

import Text.Parsec
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import Data.Maybe
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.IO.Class

import Debug.Trace (traceShow)
import Control.DeepSeq (force)



type Var = String

type HeaderSchema = (String, [Var], String)

showHeader :: HeaderSchema -> String
showHeader (l,vs,r) = unwords $ [l] ++ vs ++ [r]

getHeader :: MonadError String m =>
             FilePath -> String -> m HeaderSchema
getHeader name line = let line' = words line in
  case line' of
    []                 -> throwError $ "No header declared in `" ++ name ++"`."
    xs | length xs < 2 -> throwError $ "No delimiters declared in the header of `" ++ name ++"`."
       | otherwise -> return ( head line'
                             , init $ drop 1 line'
                             , last line'
                             )

parseBody :: Monad m => String -> ParsecT LT.Text u m String
parseBody l = do
  x <- anyChar
  xs <- manyTill anyChar $ eof <|> (return () <* (try $ string l))
  return (x:xs)

parseDelim :: Monad m => (String, String) -> ParsecT LT.Text u m String
parseDelim (l,r) = string l *> manyTill anyChar (try $ string r)

parseChunks :: Monad m => (String, String) -> ParsecT LT.Text u m [Either String String]
parseChunks (l,r) =
  many $ eitherP (parseBody l) $ parseDelim (l,r)



data ParseState = ParseState
  { absState :: Maybe Bool
  , appState :: [ExpVar]
  , depthState :: Int
  , freshAppState :: Bool
  } deriving (Show, Eq)

initParseState :: ParseState
initParseState = ParseState Nothing [] 0 False

runParse :: ( Monad m
            , MonadError String m
            ) => StateT ParseState m a
              -> m a
runParse m = evalStateT m initParseState


-- expression will need to have a binary tree such that the target of added
-- children will be explicitly declared, and certain operations can shift this
-- target up or down:
--
-- apply: /\o <- target
--      x/\x  <- previous

-- left paren:  / \
--           x/\x /\o
--                ^ still needs initial node - foldl1 style

-- right paren  x/\o  ->   /\o
--                       x/\ <- delete?


-- | Parser for expressions. Note - cannot parse @EConc@ or @EText@ constructors -
-- they are implicit, and not considered in evaluation.
parseExpr :: ( MonadState ParseState m
             , MonadError String m
             ) => [ExprTokens] -> m Exp
parseExpr tokens = undefined


  -- expr <- foldr go ((Nothing, [], 0), Nothing) tokens
  -- where
  --   go TArrow ((Just _, _, _), _) = error "Parse Error: Already ended lambda delcaration."
  --   go TArrow (_, Nothing) = error "Parse Error: Lambda abstraction without body."
  --   go TArrow ((Nothing, [], depth), Just e) = ((Just [], [], depth), Just e)
  --   go TArrow ((Nothing, as, depth), Just e) = ((Just [], [], depth), Just $ EApp (appVars as) e)
  --
  --   go TLamb ((Nothing, _, _), _) = error "Parse Error: Must end lambda declaration with `->`."
  --   go TLamb ((Just [], _, _), _) = error "Parse Error: Empty lambda declaration."
  --   go TLamb (_, Nothing) = error "Parse Error: Can't end with an empty lambda declaration."
  --   go TLamb ((Just vs, [], depth), Just e) = ((Nothing, depth), Just $ foldr EAbs e vs)
  --   go TLamb ((_, as, _), _) = error "Internal Error: No applyees should exist in a lambda declaration."
  --
  --   go (TIdent n) ((Just vs, [], depth), Just e) = ((Just $ n : vs, depth), Just e)
  --   go (TIdent n) ((Nothing, as, depth), Nothing) = ((Nothing, n:as, depth), Nothing)
  --
  --   go RParen ((Nothing, as, depth), Nothing) = ((Nothing, [], depth+1), Just $ appVars as)

parseDocument :: ( MonadIO m
                 , MonadError String m
                 ) => FilePath -> ParsecT LT.Text u m Exp
parseDocument name = do
  firstLine <- manyTill anyChar newline
  let (l,vs,r) = case runExcept $ getHeader name firstLine of
                      Right hs -> hs
                      Left err -> error err
  -- top-first, bottom-last
  (chunks :: [Either String String]) <- parseChunks (l,r)

  liftIO $ print chunks
  bodyExprFirst <- buildExpr $ head chunks
  bodyExpr <- foldM go bodyExprFirst $ tail chunks
  foldM (\acc n -> return $ EAbs n acc) bodyExpr vs
  where
    buildExpr :: ( Monad m
                 , MonadError String m
                 ) => Either String String -> ParsecT LT.Text u m Exp
    buildExpr (Left body)  = return $ EText [(name, LT.pack body)]
    buildExpr (Right exprString) = do
      ts <- lift $ runTokens (tokenize exprString)
      (ts', n) <- lift $ runStateT (group ([],ts)) 0
      if n /= 0 then throwError $ "Parser Error: Possibly mismatched brackets - `" ++ show ts ++ "`."
                else lift $ runParse (parseExpr $ fst ts')

    go acc = liftM (EConc acc) . buildExpr -- right-append to EConc


eitherP :: Monad m =>
           ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m (Either a b)
eitherP a b = (Left <$> a) <|> (Right <$> b)




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
