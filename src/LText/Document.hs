{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  #-}

module LText.Document where

import LText.Expr

import           Data.Text.Lazy       (Text)
import qualified Data.Text.Lazy    as LT
import qualified Data.Text.Lazy.IO as LT

import Control.Monad
import Control.Monad.IO.Class


data Document = Document
  { documentArity :: [Text]
  , documentBody  :: [DocumentBody]
  } deriving (Show, Eq)

data DocumentBody
  = RawText Text
  | Expression Expr
  deriving (Show, Eq)


parseDocument :: MonadParse m => LT.Text -> m Document
parseDocument ts' =
  case LT.lines ts' of
    [] -> pure $ Document [] []
    (head:body) ->
      let (l,r,hs) = parseHead head
          go :: MonadParse m => [DocumentBody] -> Text -> m [DocumentBody]
          go bs b =
            case findExpression l r b of
              Just ts -> do e <- liftIO . runParse $ LT.toStrict ts
                            pure $ bs ++ [Expression e]
              Nothing ->
                case (take (length bs - 1) bs, drop (length bs - 1) bs) of
                  (bs',[RawText b']) -> pure $ bs' ++ [RawText $ LT.unlines [b',b]]
                  _                  -> pure $ bs  ++ [RawText b]
      in  Document hs <$> foldM go [] body
  where
    findExpression :: Text -> Text -> Text -> Maybe Text
    findExpression l r ts =
      case LT.words ts of
        []    -> Nothing
        [_]   -> Nothing
        [_,_] -> Nothing
        (l':ts')
          | l' == l ->
            let (ts'',r') = (take (length ts' - 1) ts', last ts')
            in if r' == r
            then Just $ LT.unwords ts''
            else Nothing
          | otherwise -> Nothing
    parseHead :: LT.Text -> (Text, Text, [Text])
    parseHead h =
      case LT.words h of
        []    -> ("","",[])
        [_]   -> ("","",[])
        [l,r] -> (l, r, [])
        (l:hs) ->
          (l, last hs, take (length hs - 1) hs)


fromDocument :: Document -> Expr
fromDocument (Document head body) =
  foldr (Abs . LT.unpack) (go body) head
  where
    go []                = Lit ""
    go (RawText t:ts)    = Concat (Lit t) (go ts)
    go (Expression e:ts) = Concat e (go ts)


fetchDocument :: MonadParse m => FilePath -> m Expr
fetchDocument f = do
  txt <- liftIO $ LT.readFile f
  fromDocument <$> parseDocument txt
