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
  = RawText [Text]
  | Expression Expr
  deriving (Show, Eq)


parseDocument :: MonadParse m => LT.Text -> m Document
parseDocument ts =
  case LT.lines ts of
    [] -> pure $ Document [] []
    (head':body) ->
      case parseHead head' of
        Nothing       -> pure $ Document [] [RawText $! head':body]
        Just (l,r,hs) ->
          let go :: MonadParse m => [DocumentBody] -> Text -> m [DocumentBody]
              go acc b =
                case findExpression l r b of
                  Just ts' -> do
                    e <- liftIO . runParse $ LT.toStrict ts'
                    pure $ acc ++ [Expression e]

                  Nothing ->
                    case unsnoc acc of
                      Just (acc', RawText b') ->
                        pure $ acc' ++ [RawText $! b' ++ [b]]
                      _ ->
                        pure $ acc  ++ [RawText [b]]
          in  Document hs <$> foldM go [] body
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

    parseHead :: LT.Text -> Maybe (Text, Text, [Text])
    parseHead h =
      case LT.words h of
        []    -> Nothing
        [_]   -> Nothing
        [_,_] -> Nothing
        (l:hs) -> case unsnoc hs of
          Nothing      -> error "impossible state"
          Just (hs',r) -> Just (l, r, hs')



printDocument :: MonadPrettyPrint m => Maybe (Text, Text) -> Document -> m Text
printDocument mds (Document head' body) = do
  bs <- concat <$> mapM go body
  case head' of
    [] -> pure $ LT.unlines bs
    _ ->
      case mds of
        Nothing      -> throwM NoExplicitDelimiters
        Just (ld,rd) ->
          pure . LT.unlines $
              LT.unwords (ld : (head' ++ [rd]))
            : bs
  where
    go :: MonadPrettyPrint m => DocumentBody -> m [Text]
    go (RawText t)    = pure t
    go (Expression e) =
      case mds of
        Nothing      -> throwM NoExplicitDelimiters
        Just (ld,rd) -> do
          e' <- LT.pack <$> ppExpr e
          pure [ld <> " " <> e' <> " " <> rd]



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
  = ConcatExprText Expr
  | NoExplicitDelimiters
  deriving (Show, Eq, Generic)

instance Exception PrintError

handlePrintError :: PrintError -> IO a
handlePrintError e = do
  hPutStrLn stderr $
    case e of
      ConcatExprText ex ->
        "[Print Error] Can't print textual data while residually inside an expression: "
        ++ show (subLit ex) --FIXME: Need backtracing; Lit annotated with source location?
                            -- Backtracing monad?
      NoExplicitDelimiters ->
        "[Print Error] Can't render a document with residual arity without explicit\
        \ --left and --right delimiters"
  exitFailure
  where
    subLit :: Expr -> Expr
    subLit (Lit _) = Lit ["###"]
    subLit (App e1 e2) = App (subLit e1) (subLit e2)
    subLit (Concat e1 e2) = Concat (subLit e1) (subLit e2)
    subLit (Var n) = Var n
    subLit (Abs n e) = Abs n (subLit e)



toDocument :: MonadThrow m => Expr -> m Document
toDocument e =
  if not $ isPrintable e
  then throwM $ ConcatExprText e
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
  txts <- LT.lines <$> LT.readFile f
  pure $ Lit txts
