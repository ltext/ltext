module Main where

import LText.Expr
import LText.Document

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Data.Text      as T
import Data.Text.Lazy as LT



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
    e' <- runParse . T.pack =<< ppExpr e
    pure $ e' == e

printParseIsoDoc :: String -> Property
printParseIsoDoc t =
  let txt = LT.pack t
  in  ioProperty $ do
        (d,mlr) <- runParserT $ parseDocument txt
        txt'    <- printDocument mlr d
        pure $ txt == txt'
