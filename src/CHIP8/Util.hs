{-|
Module      : CHIP8.Util
Description : Defines some utility functions for dealing with Word datatypes
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}

module CHIP8.Util (
      decrementToZero
    , makeWord16
    ) where

import Data.Bits    ((.|.), shiftL)
import Data.Word    (Word8, Word16)


makeWord16 :: Word8 -> Word8 -> Word16
makeWord16 w1 w2 = ((fromIntegral w1) `shiftL` 8) .|. (fromIntegral w2)

decrementToZero :: Word16 -> Word16
decrementToZero word 
    | word > 0 = word - 1
    | otherwise = 0

