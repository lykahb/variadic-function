{-# LANGUAGE TypeApplications #-}

import Data.Function.Variadic
import Data.Function.Variadic.Utils
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "createFunction" $ do
    it "constN " $ do
      constN "one" () () `shouldBe` "one"

    it "mappendN" $ do
      mappendN [1] [2, 3] `shouldBe` [1, 2, 3]

  describe "transformFunction" $ do
    it "composeN" $ do
      let f :: Int -> Int -> Int -> String
          f = show `composeN` \a b c -> a + b + c
      f 1 2 3 `shouldBe` "6"

    it "composeN with inferred type" $ do
      let f = show `composeN` \a b c -> (a + b + c :: Int)
      f 1 2 3 `shouldBe` "6"
