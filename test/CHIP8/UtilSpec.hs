{-|
Module      : Util
Description : The utility function tests
Copyright   : (c) Chad Reynolds, 2018-2023
License     : MIT
-}
module UtilSpec
  ( spec
  ) where

import           Test.Hspec (Spec, describe, it, shouldBe)

import           Util       (addCarry, decrementToZero, makeWord16, splitWord16, subtractCarry)

spec :: Spec
spec = do
  describe "Util tests" $ do
    describe "makeWord16 tests" $ do
      it "0 and 0 to 0" $ do
        makeWord16 0x00 0x00 `shouldBe` 0x0000
      it "1 and 1 to 257" $ do
        makeWord16 0x01 0x01 `shouldBe` 0x0101
      it "255 and 255 to 65535" $ do
        makeWord16 0xFF 0xFF `shouldBe` 0xFFFF
    describe "splitWord16 tests" $ do
      it "0 to 0 and 0" $ do
        splitWord16 0x0000 `shouldBe` (0x00, 0x00)
      it "257 to 1 and 1" $ do
        splitWord16 0x0101 `shouldBe` (0x01, 0x01)
      it "65535 to 255 and 255" $ do
        splitWord16 0xFFFF `shouldBe` (0xFF, 0xFF)
    describe "decrementToZero tests" $ do
      it "1 to 0" $ do
        decrementToZero 0x01 `shouldBe` 0x00
      it "0 to 0" $ do
        decrementToZero 0x00 `shouldBe` 0x00
      it "255 to 254" $ do
        decrementToZero 0xFF `shouldBe` 0xFE
    describe "addCarry tests" $ do
      it "0 and 0 to 0 and 0" $ do
        addCarry 0x00 0x00 `shouldBe` (0x00, 0x00)
      it "255 and 1 to 0 and 1" $ do
        addCarry 0xFF 0x01 `shouldBe` (0x00, 0x01)
      it "127 and 128 to 255 and 0" $ do
        addCarry 0x7F 0x80 `shouldBe` (0xFF, 0x00)
      it "128 and 128 to 0 and 1" $ do
        addCarry 0x80 0x80 `shouldBe` (0x00, 0x01)
      it "255 and 255 to 510" $ do
        addCarry 0xFF 0xFF `shouldBe` (0xFE, 0x01)
    describe "subtractCarry tests" $ do
      it "0 and 0 to 0 and 0" $ do
        subtractCarry 0x00 0x00 `shouldBe` (0x00, 0x01)
      it "1 and 255 to 0 and 1" $ do
        subtractCarry 0x01 0xFF `shouldBe` (0x02, 0x00)
      it "255 and 255 to 0 and 1" $ do
        subtractCarry 0xFF 0xFF `shouldBe` (0x00, 0x01)
