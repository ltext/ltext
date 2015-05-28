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
  { inLambdaDec :: Bool -- no groups allowed
  , isFreshScope :: Bool -- lambda decs need to be fresh
  , exprSoFar :: Maybe Exp -- facilitates left associativity
  } deriving (Show, Eq)

initParseState :: ParseState
initParseState = ParseState False True Nothing

runParse :: ( Monad m
            , MonadError String m
            ) => StateT ParseState m a
              -> m a
runParse m = evalStateT m initParseState


-- | Parser for expressions. Note - cannot parse @EConc@ or @EText@ constructors -
-- they are implicit, and not considered in evaluation.
parseExpr :: ( MonadState ParseState m
             , MonadError String m
             ) => [ExprTokens] -> m Exp
parseExpr [] = do
  state <- get
  if | isNothing (exprSoFar state) -> throwError $ "Parser Error: Empty Sub-expression - `" ++ show state ++ "`."
     | otherwise -> return $ fromJust $ exprSoFar state
parseExpr (TLamb:xs) = do
  state <- get
  if | inLambdaDec state -> throwError $ "Parser Error: Already in lambda declaration - `" ++ show (TLamb:xs) ++ "`."
     | isFreshScope state && not (inLambdaDec state) -> do -- second condition /should/ be redundant
          put $ state {inLambdaDec = True, isFreshScope = False}
          parseExpr xs
     | isJust (exprSoFar state) -> throwError $ "Parser broken: lambda after exprSoFar - `" ++ show (TLamb:xs) ++ "`, `" ++ show state ++ "`."
     | otherwise -> throwError $ "Parser Error: Lambda declarations must be in fresh expression scope - `" ++ show (TLamb:xs) ++ "`."
parseExpr (TArrow:xs) = do
  state <- get
  if | not (inLambdaDec state) -> throwError $ "Parser Error: Not in lambda declaration - `" ++ show (TArrow:xs) ++ "`."
     | isFreshScope state -> throwError $ "Parser Error: No preceding lambda declaration - `" ++ show (TArrow:xs) ++ "`."
     | isJust (exprSoFar state) -> throwError $ "Parser broken: arrow after exprSoFar - `" ++ show (TLamb:xs) ++ "`, `" ++ show state ++ "`."
     | otherwise -> do
          put $ state {inLambdaDec = False, isFreshScope = True}
          parseExpr xs
parseExpr (TIdent n:xs) = do
  state <- get
  if | inLambdaDec state -> do
          e <- parseExpr xs
          return $ EAbs n e
     | isFreshScope state
       && isNothing (exprSoFar state) -> do
          put $ state { isFreshScope = False
                      , exprSoFar = Just $ EVar n
                      }
          parseExpr xs
     | not (isFreshScope state)
       && isJust (exprSoFar state) -> do
          put $ state {exprSoFar = Just $ EApp (fromJust $ exprSoFar state) $ EVar n}
          parseExpr xs
     | otherwise -> throwError $ "Parser broken: identifier not in lambda dec or body - `" ++ show (TIdent n:xs) ++ "`, `" ++ show state ++ "`."
parseExpr (TGroup es:xs) = do
  state <- get
  if | inLambdaDec state -> throwError $ "Parser Error: No brackets allowed in lambda declaration - `" ++ show (TGroup es:xs) ++ "`."
     | isNothing (exprSoFar state) -> do
          e <- parseExpr es
          put $ state {exprSoFar = Just e} -- should have fresh scope & not in lambda dec
          parseExpr xs
     | otherwise -> do
          let prev = exprSoFar state
          put $ state { exprSoFar = Nothing
                      , isFreshScope = True
                      , inLambdaDec = False }
          e <- parseExpr es
          state' <- get
          put $ state { exprSoFar = Just $ EApp (fromJust prev) e
                      , isFreshScope = False
                      , inLambdaDec = False }
          parseExpr xs


makeExpr :: ( Monad m
            , MonadError String m
            , MonadIO m
            ) => String -> m Exp
makeExpr s = do
  ts <- lexer s
  runParse $ parseExpr ts


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
