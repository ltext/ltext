{-# LANGUAGE
    OverloadedStrings
  , ConstraintKinds
  , DeriveGeneric
  , FlexibleContexts
  , DataKinds
  #-}

module LText.Expr where

{-
Represents the expression usable from the command line, and within
a delimitation.
-}

import Prelude hiding (lex)
import Data.Attoparsec.Text
import Data.Text as T (Text)
import qualified Data.Text.Lazy as LT
import Data.Char (isPunctuation, isSymbol, isAlphaNum)
import Text.PrettyPrint (Doc, parens, text, (<+>), nest, ($$), render)
import qualified Text.PrettyPrint as PP

import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Control.Monad.State (StateT, MonadState, put, get, evalStateT)
import Control.Monad.IO.Class (MonadIO)

import GHC.Generics (Generic)
import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure)

import Test.QuickCheck (Arbitrary (arbitrary, shrink), suchThat, sized, resize, oneof, listOf1)



data Expr
  = Abs String Expr
  | App Expr Expr
  | Var String
  | Lit { litContent :: [LT.Text], litSource :: FilePath, litInError :: Bool }
  | Concat { concatLeft :: Expr, concatRight :: Expr, concatSource :: FilePath, concatInError :: Bool }
  deriving (Show, Eq)



-- | Only considers Abs, App and Var
instance Arbitrary Expr where
  arbitrary = sized $ \n ->
    if n <= 1
    then var
    else resize (n-1) (oneof [abs', app, var]) `suchThat` (\e -> sizeOfExpr e <= 10)
    where
      sizeOfExpr :: Expr -> Int
      sizeOfExpr (Lit _ _ _) = 1
      sizeOfExpr (Var _) = 1
      sizeOfExpr (Abs _ e) = 1 + sizeOfExpr e
      sizeOfExpr (App e1 e2) = 1 + sizeOfExpr e1 + sizeOfExpr e2
      sizeOfExpr (Concat e1 e2 _ _) = 1 + sizeOfExpr e1 + sizeOfExpr e2

      term = listOf1 (arbitrary `suchThat` isFilename)
        where
          isFilename c = c /= '\\'
                      && c /= '('
                      && c /= ')'
                      && (isAlphaNum c
                      || isSymbol c
                      || isPunctuation c)
      abs' = sized $ \n -> do
        x <- term
        e <- resize (n-1) arbitrary
        pure $ Abs x e
      app = sized $ \n -> do
        e1 <- resize (n-1) arbitrary
        e2 <- resize (n-1) arbitrary
        pure $ App e1 e2
      var = do
        x <- term
        pure $ Var x

  shrink (Lit _ _ _)        = []
  shrink (Var _)        = []
  shrink (Abs _ e)      = [e]
  shrink (App e1 e2)    = [e1,e2]
  shrink (Concat e1 e2 _ _) = [e1,e2]



type MonadPrettyPrint m =
  ( MonadThrow m
  , MonadIO m
  )


-- | TODO: pretty print exceptions
ppExpr :: MonadPrettyPrint m => Expr -> m String
ppExpr e = render <$> go e
  where
    go :: MonadPrettyPrint m => Expr -> m Doc
    go e' =
      case e' of
        Abs x e'' -> do
          e''' <- go e''
          pure $ (PP.char '\\' PP.<> text x) <+> text "->"
                              $$ nest (5 + length x) e'''
        App e1 e2 ->
          let e1Hat = case e1 of
                Abs _ _ -> parens <$> go e1
                _       -> go e1
              e2Hat = case e2 of
                Abs _ _ -> parens <$> go e2
                App _ _ -> parens <$> go e2
                _       -> go e2
          in  (<+>) <$> e1Hat <*> e2Hat
        Var x ->
          pure $ text x
        Lit _ source True ->
          pure . text $ "[text from \"" ++ source ++ "\"]"
        Lit x _ _ ->
          pure . text . LT.unpack $ LT.unlines x
        Concat _ _ source True ->
          pure . text $ "[text from \"" ++ source ++ "\"]"
        Concat x y _ _ ->
          (<+>) <$> go x <*> go y


data ScopeUse = Fresh | Stale Expr
  deriving (Show, Eq)

data ParseState
  = InsideLambda   -- ^ \..->
  | Scope ScopeUse -- ^ (..)
  deriving (Show, Eq)

initParseState :: ParseState
initParseState = Scope Fresh

data ParseError
  = BracketsInsideLambda [Lexeme]
  | LambdaInsideLambda   [Lexeme]
  | LambdaInStaleScope   [Lexeme] Expr
  | ArrowWithoutLambda   [Lexeme]
  | ArrowInScope         [Lexeme]
  | EmptyExpression
  | LexerError String
  deriving (Show, Eq, Generic)

instance Exception ParseError

handleParseError :: ParseError -> IO a
handleParseError e = do
  hPutStrLn stderr $
    case e of
      BracketsInsideLambda ls ->
        "[Parse Error] Brackets are inside a lambda declaration,\
        \ with trailing token stream: " ++ show ls
      LambdaInsideLambda ls ->
        "[Parse Error] A lambda is inside a lambda declaration,\
        \ with trailing token stream: " ++ show ls
      LambdaInStaleScope ls e' ->
        "[Parse Error] A lambda is inside a stale scope,\
        \ with trailing token stream: " ++ show ls ++ " and parse state " ++ show e'
      ArrowWithoutLambda ls ->
        "[Parse Error] An arrow was found without a preceding lambda,\
        \ with trailing token stream: " ++ show ls
      ArrowInScope ls ->
        "[Parse Error] An arrow alone was found inside a function body,\
        \ with trailing token stream: " ++ show ls
      EmptyExpression ->
        "[Parse Error] Empty expression"
      LexerError err ->
        "[Lexer Error] " ++ err
  exitFailure


type MonadParse m =
  ( MonadState ParseState m
  , MonadThrow m
  , MonadIO m
  )

runParse :: Text -> IO Expr
runParse = runParserT . parseExpr


runParserT :: StateT ParseState IO a -> IO a
runParserT xs = evalStateT xs initParseState


parseExpr :: MonadParse m => Text -> m Expr
parseExpr t =
  case parseOnly lex t of
    Left err -> throwM $ LexerError err
    Right ls -> expr ls




expr :: MonadParse m => [Lexeme] -> m Expr
expr ls =
  case ls of
    [] -> do
      s <- get
      case s of
        Scope (Stale e) -> pure e
        _ -> throwM EmptyExpression
    (Lambda:ls') -> do
      s <- get
      case s of
        InsideLambda    -> throwM . LambdaInsideLambda $ Lambda : ls'
        Scope (Stale e) -> throwM $ LambdaInStaleScope (Lambda : ls') e
        Scope Fresh     -> do
          put InsideLambda
          expr ls'
    (Arrow:ls') -> do
      s <- get
      case s of
        Scope _      -> throwM . ArrowInScope $ Arrow : ls'
        InsideLambda -> do
          put $ Scope Fresh
          expr ls'
    (Ident x:ls') -> do
      s <- get
      case s of
        InsideLambda -> do
          e <- expr ls'
          pure $ Abs x e
        Scope Fresh -> do
          put . Scope . Stale $ Var x
          expr ls'
        Scope (Stale f) -> do
          put . Scope . Stale . App f $ Var x
          expr ls'
    (Bracketed bs:ls') -> do
      s <- get
      case s of
        InsideLambda -> throwM . BracketsInsideLambda $ Bracketed bs : ls'
        Scope Fresh  -> do
          e <- expr bs
          put . Scope $ Stale e
          expr ls'
        Scope (Stale f) -> do
          put $ Scope Fresh
          e <- expr bs
          put . Scope . Stale $ App f e
          expr ls'


-- * Lexing

data Lexeme
  = Lambda
  | Arrow
  | Ident String
  | Bracketed { getBracketed :: [Lexeme] }
  deriving (Show, Eq)


-- | Expects to be wrapped in parens
lex :: Parser [Lexeme]
lex = many (lambda <|> arrow <|> bracketed <|> ident)

lambda :: Parser Lexeme
lambda = do
  skipSpace
  Lambda <$ char '\\' <?> "lambda"

arrow :: Parser Lexeme
arrow = do
  skipSpace
  Arrow <$ string "->" <?> "arrow"

ident :: Parser Lexeme
ident = do
  skipSpace
  Ident <$> many1 (satisfy isFilename)
  where
    isFilename c = c /= '\\'
                && c /= '('
                && c /= ')'
                && (isAlphaNum c
                || isSymbol c
                || isPunctuation c)

bracketed :: Parser Lexeme
bracketed  = do
  skipSpace
  void (char '(') <?> "left paren"
  ls <- lex
  skipSpace
  void (char ')') <?> "right paren"
  pure $ Bracketed ls
