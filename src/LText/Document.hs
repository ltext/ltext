module LText.Document where

import LText.Expr

import Data.Text.Lazy as LT


data Document = Document
  { documentArity :: [String]
  , documentBody  :: [DocumentBody]
  } deriving (Show, Eq)

data DocumentBody
  = RawText Text
  | Expression Expr
  deriving (Show, Eq)
