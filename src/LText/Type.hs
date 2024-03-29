{-# LANGUAGE
    ConstraintKinds
  , DeriveGeneric
  , FlexibleContexts
  #-}

module LText.Type where

import Application.Types (Env (..))
import LText.Expr (Expr (..))
import LText.Document (fetchDocument)

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as HS
import Data.Maybe (fromMaybe)
import Text.PrettyPrint (text, (<+>), parens, render)
import Control.Monad (foldM)
import Control.Monad.State (StateT, MonadState, modify', get, put, evalStateT)
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader, ask)
import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)

import System.Directory (doesFileExist)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import GHC.Generics (Generic)



-- * Type Grammar

-- | We're working in an implicitly quantified prenex-polymorphic type
--   system, so trivial type expressions are also type schemes.
data Type
  = Text
  | TVar String
  | TArrow Type Type
  deriving (Show, Eq)

ppType :: Type -> String
ppType = render . go
  where
    go t =
      case t of
        Text   -> text "Text"
        TVar n -> text n
        TArrow t1 t2 ->
          let t1Hat = case t1 of
                TArrow _ _ -> parens $ go t1
                _          -> go t1
          in  t1Hat <+> text "->" <+> go t2



-- * Kit Effects

data TypeError
  = CantUnify
      { expectedType :: Type
      , givenType    :: Type
      }
  | UnboundVariable String
  | OccursCheckFailure String Type
  deriving (Show, Eq, Generic)

instance Exception TypeError

handleTypeError :: TypeError -> IO a
handleTypeError e = do
  hPutStrLn stderr $
    case e of
      CantUnify t1 t2 ->
        "[Type Error] Can't unify type " ++ ppType t1 ++ " with " ++ ppType t2
      UnboundVariable n ->
        "[Type Error] Unbound variable " ++ show n
      OccursCheckFailure n t ->
        "[Type Error] Occurs check failure: type variable " ++ show n ++ " in "
        ++ ppType t ++ ". Note: recursion is forbidden"
  exitFailure



data TypeEnv = TypeEnv
  { plaintextFiles :: HashSet FilePath
  } deriving (Show, Eq)

toTypeEnv :: Env -> TypeEnv
toTypeEnv (Env _ _ r _) = TypeEnv r

emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv HS.empty


type MonadTypecheck m =
  ( MonadState Context m
  , MonadReader TypeEnv m
  , MonadThrow m
  , MonadIO m
  )

type TypeCheckM = StateT Context (ReaderT TypeEnv IO)

runTypeCheckM :: TypeEnv -> TypeCheckM a -> IO a
runTypeCheckM te x =
  runReaderT (evalStateT x initContext) te


-- * TypeChecking

newtype Subst = Subst
  { getSubst :: HashMap String Type
  } deriving (Show, Eq)

instance Semigroup Subst where
  (Subst f) <> (Subst g) = Subst $ fmap (applySubst (Subst f)) g <> f

instance Monoid Subst where
  mempty  = Subst mempty
  mappend = (<>)


class IsType t where
  freeTVars  :: t -> HashSet String
  applySubst :: Subst -> t -> t

instance IsType a => IsType [a] where
  freeTVars    = foldMap freeTVars
  applySubst s = map (applySubst s)

instance IsType Type where
  freeTVars t =
    case t of
      Text         -> HS.empty
      TVar n       -> HS.singleton n
      TArrow t1 t2 -> freeTVars t1 <> freeTVars t2
  applySubst s t =
    case t of
      Text         -> Text
      TVar n       -> fromMaybe (TVar n) . HM.lookup n $ getSubst s
      TArrow t1 t2 -> TArrow (applySubst s t1) (applySubst s t2)

data Scheme = Scheme
  { schemeQuant :: HashSet String
  , schemeType  :: Type
  } deriving (Show, Eq)

instance IsType Scheme where
  freeTVars (Scheme qs t) =
    freeTVars t `HS.difference` qs
  applySubst (Subst s) (Scheme qs t) =
    Scheme qs $ applySubst (Subst $ foldr HM.delete s qs) t



freshTVar :: MonadTypecheck m => m Type
freshTVar = somewhatFreshTVar "a"


somewhatFreshTVar :: MonadTypecheck m => String -> m Type
somewhatFreshTVar s = do
  (Context cs i) <- get
  put . Context cs $ i + 1
  pure . TVar $ s ++ show i


mostGeneralUnifier :: MonadTypecheck m => Type -> Type -> m Subst
mostGeneralUnifier (TArrow tl1 tl2) (TArrow tr1 tr2) = do
  s1 <- mostGeneralUnifier tl1 tr1
  s2 <- mostGeneralUnifier (applySubst s1 tl2) (applySubst s1 tr2)
  pure $ s1 <> s2
mostGeneralUnifier (TVar n) t = varBind n t
mostGeneralUnifier t (TVar n) = varBind n t
mostGeneralUnifier Text Text  = pure mempty
mostGeneralUnifier t1 t2      = throwM $ CantUnify t1 t2

-- | Substitute n for t, given there's no collision
varBind :: MonadTypecheck m => String -> Type -> m Subst
varBind n t
  | t == TVar n               = pure mempty
  | n `HS.member` freeTVars t = throwM $ OccursCheckFailure n t
  | otherwise                 = pure $ Subst $ HM.singleton n t


data Context = Context
  { contextMap   :: HashMap String Scheme
  , contextFresh :: Int
  } deriving (Show, Eq)

initContext :: Context
initContext = Context
  { contextMap   = HM.empty
  , contextFresh = 0
  }


removeTVar :: String -> Context -> Context
removeTVar n (Context cs f) = Context (HM.delete n cs) f

instance IsType Context where
  freeTVars (Context cs _)    = freeTVars $ HM.elems cs
  applySubst s (Context cs f) = Context (fmap (applySubst s) cs) f

-- | Where we don't want to include variables bound by our context
quantify :: MonadTypecheck m => Type -> m Scheme
quantify t = do
  c <- get
  pure $ Scheme (freeTVars t `HS.difference` freeTVars c) t

-- | Replaces bound variables with fresh ones
unQuantify :: MonadTypecheck m => Scheme -> m Type
unQuantify (Scheme qs t) = do
  s <- Subst <$> foldM (\s q -> do n <- freshTVar
                                   pure $ HM.insert q n s) HM.empty qs
  pure $ applySubst s t


-- ** Actual Typechecking

typeOfTopLevel :: MonadTypecheck m => Expr -> m Type
typeOfTopLevel e = do
  (s,t) <- typeInfer TopLevel e
  pure $ applySubst s t


data ExprType = TopLevel | DocLevel


-- TODO: Add a flag for free variable checking or not checking for documents
typeInfer :: MonadTypecheck m => ExprType -> Expr -> m (Subst, Type)
typeInfer mode' e =
  case e of
    Lit _ _ _ -> pure (mempty, Text)
    Concat e1 e2 _ _ -> do
      -- FIXME: Probably wasteful :\
      (s1,t1) <- typeInfer mode' e1
      (s2,t2) <- typeInfer mode' e2
      s3 <- mostGeneralUnifier t1 t2
      s4 <- mostGeneralUnifier (applySubst s3 t1) Text
      pure (s4 <> s3 <> s2 <> s1, Text)
    Var x -> do
      ctx <- contextMap <$> get
      case HM.lookup x ctx of
        Nothing ->
          case mode' of
            TopLevel -> do
              exists <- liftIO $ doesFileExist x
              if exists
              then do
                isRaw <- (HS.member x . plaintextFiles) <$> ask
                if isRaw
                then pure (mempty, Text)
                else do
                  d <- liftIO $ fetchDocument x
                  typeInfer DocLevel d
              else throwM $ UnboundVariable x
            DocLevel -> throwM $ UnboundVariable x
        Just s -> do
          t <- unQuantify s
          pure (mempty, t)
    Abs n e' -> do
      t <- freshTVar
      (Context cs f) <- get
      let ctx = Context (HM.insert n (Scheme HS.empty t) $ HM.delete n cs) f
      put ctx
      (s',t') <- typeInfer mode' e'
      pure (s', TArrow (applySubst s' t) t')
    App e1 e2 -> do
      t <- freshTVar
      (s1,t1) <- typeInfer mode' e1
      modify' (applySubst s1)
      (s2,t2) <- typeInfer mode' e2
      s3 <- mostGeneralUnifier (applySubst s2 t1) (TArrow t2 t)
      pure (s3 <> s2 <> s1, applySubst s3 t)

