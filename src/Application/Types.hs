{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  #-}

module Application.Types where

import LText.Expr
import Control.Monad.Reader
import Control.Monad.IO.Class
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS


-- | More technical shared data
data Env = Env
  { topLevelExpr :: Expr
  , isTypeQuery  :: Bool
  , rawTerms     :: HashSet FilePath
  } deriving (Eq, Show)

isRaw :: MonadApp m => FilePath -> m Bool
isRaw file = do
  raws <- rawTerms <$> ask
  pure $ HS.member file raws


type MonadApp m =
  ( MonadIO m
  , MonadReader Env m
  )


type AppM = ReaderT Env IO

runAppM :: Env -> AppM a -> IO a
runAppM e = flip runReaderT e
