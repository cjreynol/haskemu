{-|
Module      : Opcode
Description : The opcode processing function tests
Copyright   : (c) Chad Reynolds, 2023
License     : MIT
-}

module OpcodeSpec (
  spec
) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Opcode     (OpcodeComponents(..), createOpcodeComponents, decodeOpcode)

spec :: Spec
spec = do
  describe "Opcode tests" $ do
    describe "createOpcodeComponents tests" $ do
      it "test" $ do
        createOpcodeComponents 0xAB 0xCD `shouldBe` OpcodeComponents 0xAB 0xCD 0x0A 0x0B 0x0C 0x0D 0x0BCD
    describe "decodeOpcode' tests" $ do
      it "TODO CJR: add tests verifying a selection of commands" $ do
        True `shouldBe` True
