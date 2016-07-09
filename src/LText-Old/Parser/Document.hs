{-# LANGUAGE
    ScopedTypeVariables
  , FlexibleContexts
  , MultiWayIf
  #-}

module LText.Parser.Document where

import LText.Parser.Expr
import LText.Internal.Expr

import Text.Parsec
import qualified Data.Text.Lazy as LT

import Data.List (groupBy)
import Control.Monad.State
import Control.Monad.Except



type Var = String

type HeaderSchema = (String, [Var], String)


getHeader :: String -> Maybe HeaderSchema
getHeader line =
  let  line' = words line in
  case line' of
    []                 -> Nothing
    xs | length xs < 2 -> Nothing
       | otherwise     -> Just ( head line'
                               , init (drop 1 line')
                               , last line'
                               )

parseDelim :: Monad m => (String, String) -> ParsecT LT.Text u m String
parseDelim (l,r) = try (string l) *> manyTill anyChar (try $ string r)


parseDocument :: ( MonadIO m
                 , MonadError String m
                 ) => FilePath -> LT.Text -> m Expr
parseDocument name input = do
  let input' = LT.lines input

  case input' of
    [] -> pure (EText [(name, input)])
    _  -> case getHeader $ LT.unpack $ head input' of
            Nothing       -> pure (EText [(name, LT.unlines $ tail input')])
            Just (l,vs,r) -> pure (go (\e -> foldr EAbs e vs) (l,r) $ tail input')
  where
    go :: (Expr -> Expr) -> (String, String) -> [LT.Text] -> Expr
    go header (l,r) content =
      let groupedContent :: [[LT.Text]]
          groupedContent = groupBy (\x y -> not (hasDelims x) && not (hasDelims y)) content
      in header (process groupedContent)
      where
        process :: [[LT.Text]] -> Expr
        process [] = error "Error: empty list after grouping."
        process [chunk]
          | all (not . hasDelims) chunk = EText [(name, LT.unlines chunk)]
          | otherwise                   = runStuffT name l r chunk
        process (chunk:xs)
          | all (not . hasDelims) chunk = EConc (EText [(name, LT.unlines chunk)]) (process xs)
          | otherwise                   = EConc (runStuffT name l r chunk) (process xs)

        hasDelims :: LT.Text -> Bool
        hasDelims ln = LT.pack l `LT.isInfixOf` ln
                    && LT.pack r `LT.isInfixOf` ln


runStuffT :: SourceName -> String -> String -> [LT.Text] -> Expr
runStuffT name l r chunk =
  case parse (parseDelim (l,r)) name (head chunk) of
     Left _ -> EText [(name, LT.unlines chunk)]
     Right s -> case runExcept $ makeExpr s of
       Left err -> error err
       Right e  -> e
