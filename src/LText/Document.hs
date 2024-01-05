{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , DeriveGeneric
  , DataKinds
  , NamedFieldPuns
  , RecordWildCards
  , LambdaCase
  #-}

module LText.Document where

import LText.Expr (Expr (..), MonadParse, MonadPrettyPrint, runParserT, runParse, ppExpr)

import           Data.Text.Lazy       (Text)
import qualified Data.Text.Lazy    as LT
import qualified Data.Text.Lazy.IO as LT

import Data.Char (isAlphaNum)
import Data.List.Extra (unsnoc)
import Control.Monad (guard, foldM)
import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Control.Monad.IO.Class (liftIO)

import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure)
import GHC.Generics (Generic)

import Test.QuickCheck (Arbitrary (shrink, arbitrary), suchThat, oneof, listOf1)
import Test.QuickCheck.Instances ()


-- | A parsed document
data Document = Document
  { documentArity :: [Text]
    -- ^ Arity of the document - the parameters of the function, where each
    -- entry is the name of the term.
  , documentBody  :: [DocumentBody]
    -- ^ The function's body
  } deriving (Show, Eq)

instance Arbitrary Document where
  arbitrary = do
    documentArity <- fmap LT.pack <$>
      listOf1 (listOf1 (arbitrary `suchThat` isAlphaNum))
    documentBody <- listOf1 arbitrary
    pure $ Document {documentArity, documentBody}
 -- shrink (Document hs body) =
 --   Document <$> shrink hs <*> shrink body


-- | The body of a document is either a block of raw text, or an ltext expression.
data DocumentBody
  = RawText [Text]
  | Expression Expr
  deriving (Show, Eq)

instance Arbitrary DocumentBody where
  arbitrary = oneof
    [ do
      ls <- listOf1 (listOf1 (arbitrary `suchThat` isAlphaNum))
      pure . RawText $ LT.pack <$> ls
    , Expression <$> arbitrary
    ]
  shrink (Expression e) = Expression <$> shrink e
  shrink (RawText ts)   = RawText    <$> shrink ts


-- | Concatenates adjacent 'RawText' blocks
repackDocument :: [DocumentBody] -> [DocumentBody]
repackDocument ds =
  foldl go [] ds
  where
    go :: [DocumentBody] -> DocumentBody -> [DocumentBody]
    go acc l =
      case (unsnoc acc, l) of
        (Just (acc', RawText t), RawText t') -> acc' ++ [RawText $! t ++ t']
        _ -> acc ++ [l]


-- | Takes a raw text file and returns the parsed document, and left and right delimiters if it
-- has arity.
parseDocument :: MonadParse m
              => LT.Text -- ^ Document content
              -> m (Document, Maybe (Text, Text))
parseDocument ts =
  case LT.lines ts of
    [] -> pure (Document {documentArity = [], documentBody = []}, Nothing)
    bodyWithHead@(head':body) ->
      case parseHead head' of
        Nothing ->
          -- Document has 0 arity
          pure (Document {documentArity = [], documentBody = [RawText bodyWithHead]}, Nothing)
        Just (l, r, documentArity) ->
          let go :: MonadParse m => [DocumentBody] -> Text -> m [DocumentBody]
              go acc b =
                case findExpression b of
                  -- if an expression exists, parse it
                  Just ts' -> do
                    e <- liftIO . runParse $ LT.toStrict ts'
                    pure $ acc ++ [Expression e]

                  Nothing ->
                    case unsnoc acc of
                      -- Add this line of text to the previous block, if it was text as well
                      Just (acc', RawText b') ->
                        pure $ acc' ++ [RawText $! b' ++ [b]]
                      _ ->
                        pure $ acc  ++ [RawText [b]]
                where
                  -- Determines whether or not a line has an expression within it, and extracts it
                  findExpression :: Text -> Maybe Text
                  findExpression ts' =
                    case LT.words ts' of
                      []    -> Nothing
                      [_]   -> Nothing
                      [_,_] -> Nothing
                      (l':ts'') -> do
                        (ts''',r') <- unsnoc ts''
                        guard $ r' == r && l' == l
                        -- FIXME why did I undo the tokens?
                        Just $ LT.unwords ts'''
          in  do documentBody <- foldM go [] body
                 pure (Document {documentArity, documentBody}, Just (l,r))
  where

    -- interprets the first line of a document into its left delimiter, right delimiter, and
    -- parameters, respectively.
    parseHead :: LT.Text -> Maybe (Text, Text, [Text])
    parseHead h =
      case LT.words h of
        []    -> Nothing
        [_]   -> Nothing
        [_,_] -> Nothing
        (l:hs) -> case unsnoc hs of
          Nothing      -> error "impossible state"
          Just (hs',r) -> Just (l, r, hs')



printDocument :: MonadPrettyPrint m
              => Maybe (Text, Text) -- ^ Explicitly supplied delimiters
              -> Document -> m Text
printDocument mds Document{..} = do
  bs <- concat <$> mapM go documentBody
  case documentArity of
    [] -> pure $ LT.unlines bs
    _ ->
      case mds of
        Nothing      -> throwM NoExplicitDelimiters
        Just (ld,rd) ->
          pure . LT.unlines $
              LT.unwords (ld : (documentArity ++ [rd])) : bs
  where
    go :: MonadPrettyPrint m => DocumentBody -> m [Text]
    go (RawText t)    = pure t
    go (Expression e) =
      case mds of
        Nothing      -> throwM NoExplicitDelimiters
        Just (ld,rd) -> do
          -- expressions are pretty printed, then placed within delimiters
          e' <- LT.pack <$> ppExpr e
          pure [ld <> " " <> e' <> " " <> rd]


-- | Given a document, generate an expression (without thinking too hard about it)
fromDocument :: FilePath -- ^ Name of source file
             -> Document
             -> Expr
fromDocument source Document{..} =
  foldr (Abs . LT.unpack) (go documentBody) documentArity
  where
    go (RawText t:ts)    = Concat (Lit t source False) (go ts) source False
    go (Expression e:ts) = Concat e (go ts) source False
    go [RawText t]       = Lit t source False
    go [Expression e]    = e
    go []                = Lit [] source False


data PrintError
  = ConcatExprText Expr -- ^ Represents a scenario where a 'Lit' is inside a 'Abs' or 'App'.
  | NoExplicitDelimiters
  deriving (Show, Eq, Generic)

instance Exception PrintError

data PrintabilityMode
  = InsideConcat
  | InsideExpr


decorateUnprintable :: Expr -> Expr
decorateUnprintable = go Nothing
  where
    go Nothing = \case
      Concat e1 e2 s _ ->
        Concat (go (Just InsideConcat) e1) (go (Just InsideConcat) e2) s False
      Abs n e ->
        Abs n (go Nothing e)
      App e1 e2 ->
        App (go Nothing e1) (go Nothing e2)
      e -> e
    go (Just InsideConcat) = \case
      Concat e1 e2 s _ ->
        Concat (go (Just InsideConcat) e1) (go (Just InsideConcat) e2) s False
      Abs n e ->
        Abs n (go (Just InsideExpr) e)
      App e1 e2 ->
        App (go (Just InsideExpr) e1) (go (Just InsideExpr) e2)
      e -> e
    go (Just InsideExpr) = \case
      Concat l r s _ -> Concat l r s True
      Lit t s _ -> Lit t s True
      Abs n e -> Abs n (go (Just InsideExpr) e)
      App e1 e2 -> App (go (Just InsideExpr) e1) (go (Just InsideExpr) e2)
      e -> e

isAnyUnprintable :: Expr -> Bool
isAnyUnprintable = \case
  Concat _ _ _ True -> True
  Lit _ _ True -> True
  Concat e1 e2 _ _ -> isAnyUnprintable e1 || isAnyUnprintable e2
  Abs _ e -> isAnyUnprintable e
  App e1 e2 -> isAnyUnprintable e1 || isAnyUnprintable e2
  _ -> False

handlePrintError :: PrintError -> IO a
handlePrintError e = do
  case e of
    ConcatExprText ex -> do
      err <- ppExpr (decorateUnprintable ex)
      hPutStrLn stderr $ "[Print Error] Can't print textual data while inside an expression: \n\n"
        ++ err ++ "\n\n...cannot be rendered to a file."
    NoExplicitDelimiters ->
      hPutStrLn stderr "[Print Error] Can't render a document with arity without explicit --left and --right delimiters"
  exitFailure



toDocument :: MonadThrow m => Expr -> m Document
toDocument e =
  if isAnyUnprintable (decorateUnprintable e)
  then throwM $ ConcatExprText e
  else case getInitArity e of
    (documentArity, e') -> pure Document {documentArity, documentBody = getBody e'}
  where
    getBody :: Expr -> [DocumentBody]
    getBody =
      \case
        Lit t _ _    -> [RawText t]
        Concat e1 e2 _ _ -> getBody e1 ++ getBody e2
        e''          -> [Expression e'']

    -- Extracts the top-level parameters from an expression, if it's an abstraction
    getInitArity :: Expr -> ([Text], Expr)
    getInitArity =
      \case
        Abs n e'' -> let (hs          , e''') = getInitArity e''
                     in  (LT.pack n:hs, e''')
        e'        -> ([], e')


fetchDocument :: FilePath -> IO Expr
fetchDocument f = do
  txt   <- LT.readFile f
  (d,_) <- runParserT $ parseDocument txt
  pure $ fromDocument f d

rawDocument :: FilePath -> IO Expr
rawDocument f = do
  txts <- LT.lines <$> LT.readFile f
  pure $ Lit txts f False
