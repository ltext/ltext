{-# LANGUAGE
    ConstraintKinds
  , DeriveGeneric
  , FlexibleContexts
  #-}

module LText.Type where

import LText.Expr

import Control.Monad.State
import Control.Monad.Catch
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import GHC.Generics


-- | We're working in an implicitly quantified prenex-polymorphic type
--   system, so trivial type expressions are also type schemes.
data Type
  = Text
  | TVar String
  | TArrow Type Type
  deriving (Show, Eq)


-- | Stateless unification (only throws errors).
--   If it's acceptable, then return the most constrained
unify :: MonadTypecheck m => Type -> Type -> m Type
unify expectation given =
  let typeError = CantUnify { expectedType = expectation, givenType = given }
  in  case expectation of
        TVar _ -> pure given
        Text ->
          case given of
            Text      -> pure given
            TVar _    -> pure expectation
            TArrow _ _ -> throwM typeError
        TArrow e1 e2 ->
          case given of
            Text   -> throwM typeError
            TVar _ -> pure expectation
            TArrow e1' e2' -> do
              e1Hat <- unify e1 e1'
              e2Hat <- unify e2 e2'
              pure $ TArrow e1Hat e2Hat

data Context = Context
  { contextMap   :: HashMap String Type
  , contextFresh :: Int
  } deriving (Show, Eq)

embedContextMap :: MonadTypecheck m => HashMap String Type -> m ()
embedContextMap ctx = do
  cs <- get
  put $ cs { contextMap = ctx }

embedContextFresh :: MonadTypecheck m => Int -> m ()
embedContextFresh i = do
  cs <- get
  put $ cs { contextFresh = i }

data TypeError
  = CantUnify
      { expectedType :: Type
      , givenType    :: Type
      }
  | UnboundVariable String
  | TypecheckerInconsistent String -- should never throw
  deriving (Show, Eq, Generic)

instance Exception TypeError


type MonadTypecheck m =
  ( MonadState Context m
  , MonadThrow m
  )


freshTVar :: MonadTypecheck m => m Type
freshTVar = somewhatFreshTVar "a"


somewhatFreshTVar :: MonadTypecheck m => String -> m Type
somewhatFreshTVar s = do
  i <- contextFresh <$> get
  embedContextFresh $ i + 1
  pure . TVar $ s ++ show i




typeInfer :: MonadTypecheck m => Type -> Expr -> m Type
typeInfer atLeast e =
  case e of
    Var x -> do
      ctx <- contextMap <$> get
      case HM.lookup x ctx of
        Nothing -> throwM $ UnboundVariable x
        Just s  -> unify atLeast s
    Abs x e' -> do
      here <- TArrow <$> freshTVar <*> freshTVar
      -- partial, but still guaranteed to be TArrow
      (TArrow xS atLeast') <- unify atLeast here

      ctx <- contextMap <$> get
      let mAlpha = HM.lookup x ctx -- alpha equivalence
          ctx'   = HM.insert x xS $ HM.delete x ctx
      embedContextMap ctx'

      e'Type <- typeInfer atLeast' e'

      ctxBody <- contextMap <$> get

      let ctxPostAlpha =
            case mAlpha of
              Nothing        -> HM.delete x ctxBody -- closure
              Just xSExisted -> HM.insert x xSExisted ctxBody -- overwrite

      embedContextMap ctxPostAlpha

      xType <- case HM.lookup x ctxBody of
                 Nothing -> throwM $ TypecheckerInconsistent x
                 Just t  -> pure t

      pure $ TArrow xType e'Type
    App e1 e2 -> do
      argFresh  <- freshTVar
      argActual <- typeInfer argFresh e2

      let funAtLeast = TArrow argActual atLeast

      -- Partial but... should be guaranteed?
      (TArrow _ resultType) <- typeInfer funAtLeast e1

      pure resultType
