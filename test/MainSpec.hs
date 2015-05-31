{-# LANGUAGE
    FlexibleContexts
  #-}

module MainSpec (main, spec) where

import LText.Parser.Lexer

import Test.Hspec

import Control.Monad.Except

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Lexing" $ do
    it "should tokenize lambda literals" $ do
      fromError (lexer "\\x -> x") `shouldBe` [TLamb,TIdent "x",TArrow,TIdent "x"]
      fromError (lexer "\\x y -> x y") `shouldBe` [TLamb,TIdent "x",TIdent "y",TArrow,TIdent "x",TIdent "y"]
      fromError (lexer "\\x y -> (x y)") `shouldBe` [TLamb,TIdent "x",TIdent "y",TArrow,TGroup [TIdent "x",TIdent "y"]]
      fromError (lexer "\\x y -> ((x y))") `shouldBe` [TLamb,TIdent "x",TIdent "y",TArrow,TGroup [TGroup [TIdent "x",TIdent "y"]]]
    


fromError :: Except String a -> a
fromError mx = case runExcept mx of
  Left err -> error err
  Right a -> a
