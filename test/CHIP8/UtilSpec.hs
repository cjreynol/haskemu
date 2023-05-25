{-|
Module      : Util
Description : The utility function tests
Copyright   : (c) Chad Reynolds, 2018-2023
License     : MIT
-}

module UtilSpec (
  spec
) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Util (decrementToZero)

spec :: Spec
spec = do
  describe "Util tests" $ do
    describe "decrementToZero tests" $ do
      it "placeholder" $ do
        decrementToZero 0x01 `shouldBe` 0x00
