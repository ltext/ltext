module Main where

import LText.Expr

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Data.Text as T



main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "LText test suite"
  [ testGroup "Expr"
      [ QC.testProperty "Print/Parse Iso" printParseIso
      ]
  ]


printParseIso :: Expr -> Property
printParseIso e =
  ioProperty $ do
    print e
    e' <- runParse . T.pack =<< ppExpr e
    pure $ e' == e
