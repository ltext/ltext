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
import Data.List.Extra (unsnoc)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import System.IO
import System.Exit
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
parseDocument ts =
  case LT.lines ts of
    [] -> pure $ Document [] []
    (head':body) ->
      let (l,r,hs) = parseHead head'

          go :: MonadParse m => [DocumentBody] -> Text -> m [DocumentBody]
          go acc b =
            case findExpression l r b of
              Just ts' -> do
                e <- liftIO . runParse $ LT.toStrict ts'
                pure $ acc ++ [Expression e]

              Nothing ->
                case unsnoc acc of
                  Just (acc', RawText b') ->
                    pure $ acc' ++ [RawText $ b' <> "\n" <> b]
                  _ ->
                    pure $ acc  ++ [RawText b]

      in if head' == ""
      then pure $ Document [] [RawText ts]
      else Document hs <$> foldM go [] body
  where
    findExpression :: Text -> Text -> Text -> Maybe Text
    findExpression l r ts' =
      case LT.words ts' of
        []    -> Nothing
        [_]   -> Nothing
        [_,_] -> Nothing
        (l':ts'')
          | l' /= l -> Nothing
          | otherwise -> do
            (ts''',r') <- unsnoc ts''
            guard $ r' == r
            Just $ LT.unwords ts'''

    parseHead :: LT.Text -> (Text, Text, [Text])
    parseHead h =
      case LT.words h of
        []    -> ("","",[])
        [_]   -> ("","",[])
        [l,r] -> (l, r, [])
        (l:hs) -> case unsnoc hs of
          Nothing      -> error "impossible state"
          Just (hs',r) -> (l, r, hs')


printDocument :: MonadPrettyPrint m => Maybe (Text, Text) -> Document -> m Text
printDocument mds (Document head' body) = do
  bs <- mapM go body
  case head' of
    [] -> pure $ LT.unlines bs
    _ ->
      case mds of
        Nothing      -> throwM NoExplicitDelimiters
        Just (ld,rd) ->
          pure . LT.intercalate "\n" $
              LT.unwords (ld : (head' ++ [rd]))
            : bs
  where
    go :: MonadPrettyPrint m => DocumentBody -> m Text
    go (RawText t)    = pure t
    go (Expression e) =
      case mds of
        Nothing      -> throwM NoExplicitDelimiters
        Just (ld,rd) -> do
          e' <- LT.pack <$> ppExpr e
          pure $ ld <> " " <> e' <> " " <> rd


fromDocument :: Document -> Expr
fromDocument (Document head' body) =
  foldr (Abs . LT.unpack) (go body) head'
  where
    -- WARNING: partial; however, every text file is guaranteed to have at least
    -- one line.
    go (RawText t:[])    = Lit t
    go (RawText t:ts)    = Concat (Lit t) (go ts)
    go (Expression e:[]) = e
    go (Expression e:ts) = Concat e (go ts)
    go _                 = error "Text file without any text"


data PrintError
  = ConcatExprText
  | NoExplicitDelimiters
  deriving (Show, Eq, Generic)

instance Exception PrintError

handlePrintError :: PrintError -> IO a
handlePrintError e = do
  hPutStrLn stderr $
    case e of
      ConcatExprText ->
        "[Print Error] Can't print textual data while residually inside an expression"
      NoExplicitDelimiters ->
        "[Print Error] Can't render a document with residual arity without explicit\
        \ --left and --right delimiters"
  exitFailure




toDocument :: MonadThrow m => Expr -> m Document
toDocument e =
  if not $ isPrintable e
  then throwM ConcatExprText
  else case getInitArity e of
    (hs,e') -> pure . Document hs $ getBody e'
  where
    getBody :: Expr -> [DocumentBody]
    getBody e' =
      case e' of
        Lit t        -> [RawText t]
        Concat e1 e2 -> getBody e1 ++ getBody e2
        e''          -> [Expression e'']

    getInitArity :: Expr -> ([Text], Expr)
    getInitArity e' =
      case e' of
        Abs n e'' -> let (hs          , e''') = getInitArity e''
                     in  (LT.pack n:hs, e''')
        e''       -> ([], e'')

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



fetchDocument :: FilePath -> IO Expr
fetchDocument f = do
  txt <- LT.readFile f
  d   <- runParserT $ parseDocument txt
  pure $ fromDocument d

rawDocument :: FilePath -> IO Expr
rawDocument f = do
  txt <- LT.readFile f
  pure $ Lit txt
