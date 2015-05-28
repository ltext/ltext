{-# LANGUAGE
    ScopedTypeVariables
  , FlexibleContexts
  , MultiWayIf
  #-}

module LText.Parser.Document where

import LText.Parser.Lexer
import LText.Parser.Expr
import LText.Internal.Expr

import Text.Parsec
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import Data.Maybe
import Data.List (groupBy)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.IO.Class


type Var = String

type HeaderSchema = (String, [Var], String)


getHeader :: FilePath -> String -> Maybe HeaderSchema
getHeader name line =
  let line' = words line in
  case line' of
    []                 -> Nothing
    xs | length xs < 2 -> Nothing
       | otherwise -> return ( head line'
                             , init $ drop 1 line'
                             , last line'
                             )


parseDelim :: Monad m => (String, String) -> ParsecT LT.Text u m String
parseDelim (l,r) = try (string l) *> manyTill anyChar (try $ string r)


eitherP :: Monad m =>
           ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m (Either a b)
eitherP a b = (Left <$> a) <|> (Right <$> b)


parseDocument :: ( MonadIO m
                 , MonadError String m
                 ) => FilePath -> LT.Text -> m Expr
parseDocument name input = do
  let input' = LT.lines input

  case input' of
    [] -> return $ EText [(name, input)]
    _  -> case getHeader name $ LT.unpack $ head input' of
      Nothing -> return $ EText [(name, input)]
      Just (l,vs,r) -> return $
        go (\e -> foldr EAbs e vs) (l,r) $ tail input'


  where
    go :: (Expr -> Expr) -> (String, String) -> [LT.Text] -> Expr
    go header (l,r) lines =
      let lines' :: [[LT.Text]]
          lines' = groupBy (\x y -> not (hasDelims x) && not (hasDelims y)) lines
      in

      header $ process lines'
      where
        process :: [[LT.Text]] -> Expr
        process [] = error "Error: empty list after grouping."
        process [chunk] | length chunk > 1 = EText [(name, LT.unlines chunk)]
                        | otherwise = case parse (parseDelim (l,r)) name $ head chunk of
                            Left _ -> EText [(name, LT.unlines chunk)]
                            Right s -> case runExcept $ makeExpr s of
                              Left err -> error err
                              Right e -> e
        process (chunk:xs) | length chunk > 1 = EConc (EText [(name, LT.unlines chunk)]) $ process xs
                           | otherwise = case parse (parseDelim (l,r)) name $ head chunk of
                                Left err -> EText [(name, LT.unlines chunk)]
                                Right s -> case runExcept $ makeExpr s of
                                  Left err -> error err
                                  Right e -> EConc e $ process xs

        hasDelims :: LT.Text -> Bool
        hasDelims ln = LT.pack l `LT.isInfixOf` ln
                    && LT.pack r `LT.isInfixOf` ln
