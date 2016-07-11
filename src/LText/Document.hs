{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , DeriveGeneric
  #-}

module LText.Document where

import LText.Expr

import           Data.Text.Lazy       (Text)
import qualified Data.Text.Lazy    as LT
import qualified Data.Text.Lazy.IO as LT

import Data.Monoid
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import GHC.Generics


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


printDocument :: MonadPrettyPrint m => Maybe (Text, Text) -> Document -> m Text
printDocument mds (Document head body) = do
  bs <- mapM go body
  case head of
    [] -> pure $ LT.unlines bs
    hs ->
      case mds of
        Nothing -> throwM NoExplicitDelimiters
        Just (ld,rd) ->
          pure . LT.unlines $
              LT.unwords (ld : (head ++ [rd]))
            : bs
  where
    go :: MonadPrettyPrint m => DocumentBody -> m Text
    go (RawText t)    = pure t
    go (Expression e) =
      case mds of
        Nothing -> throwM NoExplicitDelimiters
        Just (ld,rd) -> do
          e' <- LT.pack <$> ppExpr e
          pure $ ld <> " " <> e' <> " " <> rd


fromDocument :: Document -> Expr
fromDocument (Document head body) =
  foldr (Abs . LT.unpack) (go body) head
  where
    go []                = Lit ""
    go (RawText t:ts)    = Concat (Lit t) (go ts)
    go (Expression e:ts) = Concat e (go ts)


data PrintError
  = ConcatExprText
  | NoExplicitDelimiters
  deriving (Show, Eq, Generic)

instance Exception PrintError


toDocument :: MonadThrow m => Expr -> m Document
toDocument e =
  if not $ isPrintable e
  then throwM ConcatExprText
  else case getInitArity e of
    (hs,e') -> pure . Document hs $ getBody e'
  where
    getBody :: Expr -> [DocumentBody]
    getBody e =
      case e of
        Lit t        -> [RawText t]
        Concat e1 e2 -> getBody e1 ++ getBody e2
        e'           -> [Expression e']

    getInitArity :: Expr -> ([Text], Expr)
    getInitArity e =
      case e of
        Abs n e' -> let (hs          , e'') = getInitArity e'
                    in  (LT.pack n:hs, e'')
        e'       -> ([], e')

    isPrintable :: Expr -> Bool
    isPrintable = not . hasConcatAbsLit
    hasConcatAbsLit :: Expr -> Bool
    hasConcatAbsLit = go Nothing
      where
        go :: Maybe PrintabilityMode -> Expr -> Bool
        go Nothing e =
          case e of
            Var _        -> False
            Lit _        -> False
            Abs _ e'     -> go Nothing e'
            App e1 e2    -> go Nothing e1 || go Nothing e2
            Concat e1 e2 -> go (Just InsideConcat) e1
                         || go (Just InsideConcat) e2
        go (Just InsideConcat) e =
          case e of
            Lit _        -> False
            Var _        -> False
            Abs _ e'     -> go (Just InsideExpr) e'
            App e1 e2    -> go (Just InsideExpr) e1
                         || go (Just InsideExpr) e2
            Concat e1 e2 -> go (Just InsideConcat) e1
                         || go (Just InsideConcat) e2
        go (Just InsideExpr) e =
          case e of
            Lit _        -> True
            Concat _ _   -> True
            Var _        -> False
            Abs _ e'     -> go (Just InsideExpr) e'
            App e1 e2    -> go (Just InsideExpr) e1
                         || go (Just InsideExpr) e2

data PrintabilityMode
  = InsideConcat
  | InsideExpr



fetchDocument :: MonadParse m => FilePath -> m Expr
fetchDocument f = do
  txt <- liftIO $ LT.readFile f
  fromDocument <$> parseDocument txt
