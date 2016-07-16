module Main where

import LText.Expr
import LText.Document
import LText.Eval
import LText.Type

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Instances

import Data.Text      as T  hiding (length, all)
import Data.Text.Lazy as LT hiding (length, all)
import Data.Char



main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "LText test suite"
  [ testGroup "Expr"
      [ QC.testProperty "Print/Parse Iso" printParseIso
      , QC.testProperty "Idempotent Eval" idempotentEval
      ]
  , testGroup "Document"
      [ QC.testProperty "Print/Parse Iso" printParseIsoDoc
      ]
  , testGroup "Type"
      [ QC.testProperty "Consistency" consistentType
      ]
  ]


printParseIso :: Expr -> Property
printParseIso e =
  ioProperty $ do
    e' <- runParse . T.pack =<< ppExpr e
    pure $ e' == e


idempotentEval :: Expr -> Bool
idempotentEval e =
  evaluate e == evaluate (evaluate e)



newtype Delims = Delims
  { getDelims :: (LT.Text, LT.Text)
  } deriving (Show, Eq)

instance Arbitrary Delims where
  arbitrary = do
    l <- arbitrary `suchThat` (\ls -> all isAlphaNum ls
                                   && length ls > 1)
    r <- arbitrary `suchThat` (\rs -> all isAlphaNum rs
                                   && length rs > 1)
    pure $ Delims (LT.pack l, LT.pack r)


printParseIsoDoc :: (Document, Delims) -> Property
printParseIsoDoc (Document head body, Delims lr) =
  let d = Document head $ repackDocument body
  in  ioProperty $ do
        txt       <- printDocument (Just lr) d
        (d',mlr') <- runParserT $ parseDocument txt
        pure $ case mlr' of
          Nothing  -> d == d'
          Just lr' -> d == d' -- && lr == lr'


consistentType :: Expr -> Property
consistentType e =
  ioProperty $ do
    t1 <- runTypeCheckM emptyTypeEnv $ typeOfTopLevel e
    t2 <- runTypeCheckM emptyTypeEnv . typeOfTopLevel $ evaluate e
    pure $ t1 == t2
