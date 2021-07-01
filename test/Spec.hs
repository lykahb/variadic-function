{-# LANGUAGE TypeApplications #-}

import Data.Function.Variadic
import Data.Function.Variadic.Utils
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Variadic function" $ do
    it "transforms function" $ do
      let f :: Int -> Int -> Int -> String
          f = show `composeN` \a b c -> a + b + c
      f 1 2 3 `shouldBe` "6"
    it "transforms function with inferred type" $ do
      let f = show `composeN` \a b c -> (a + b + c :: Int)
      f 1 2 3 `shouldBe` "6"
