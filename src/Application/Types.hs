{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  #-}

module Application.Types where

import LText.Expr
import Control.Monad.Reader
import Control.Monad.Catch
import Data.HashSet (HashSet)


-- | More technical shared data
data Env = Env
  { topLevelExpr :: Expr
  , isTypeQuery  :: Bool
  , rawTerms     :: HashSet FilePath
  , delims       :: Maybe (String, String)
  } deriving (Eq, Show)


type MonadApp m =
  ( MonadIO m
  , MonadReader Env m
  , MonadThrow m
  )


type AppM = ReaderT Env IO

runAppM :: Env -> AppM a -> IO a
runAppM e = flip runReaderT e
