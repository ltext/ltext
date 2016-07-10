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
import Data.Char
import Text.PrettyPrint hiding (char)
import qualified Text.PrettyPrint as PP

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State

import GHC.Generics
import System.IO
import System.Exit

import Test.QuickCheck
import Test.QuickCheck.Combinators



data Expr
  = Abs String Expr
  | App Expr Expr
  | Var String
  deriving (Show, Eq)


instance Arbitrary Expr where
  arbitrary = oneof [abs, app, var]
    where
      abs = do
        (Between x) <- arbitrary `suchThat` (\(Between x') -> all isAlphaNum x')
                       :: Gen (Between 1 5 [] Char)
        e <- arbitrary
        pure $ Abs x e
      app = do
        e1 <- arbitrary
        e2 <- arbitrary
        pure $ App e1 e2
      var = do
        (Between x) <- arbitrary `suchThat` (\(Between x') -> all isAlphaNum x')
                       :: Gen (Between 1 5 [] Char)
        pure $ Var x



ppExpr :: Expr -> String
ppExpr e = render (go e)
  where
    go e' =
      case e' of
        Abs x e'' ->
          PP.char '\\' <> text x <+> text "->"
                       $$ nest (5 + length x) (go e'')
        App e1 e2 ->
          let e1Hat = case e1 of
                Abs _ _ -> parens (go e1)
                _       -> go e1
              e2Hat = case e2 of
                Abs _ _ -> parens (go e2)
                App _ _ -> parens (go e2)
                _       -> go e2
          in  e1Hat <+> e2Hat
        Var x ->
          text x


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
      LambdaInStaleScope ls e ->
        "[Parse Error] A lambda is inside a stale scope,\
        \ with trailing token stream: " ++ show ls ++ " and parse state " ++ show e
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
  )

runParse :: Text -> IO Expr
runParse = handle handleParseError . runParserT . parseExpr


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
