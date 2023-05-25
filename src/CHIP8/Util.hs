{-|
Module      : Util
Description : Defines some utility functions for dealing with Word datatypes
Copyright   : (c) Chad Reynolds, 2018-2023
License     : MIT
-}

module Util (
      addCarry
    , decrementToZero
    , makeWord16
    , splitWord16
    , subtractCarry
    ) where

import Data.Bits    ((.&.), (.|.), shiftL, shiftR)
import Data.Word    (Word8, Word16)


-- | Combine the single bytes into a single 2-byte value.
makeWord16 :: Word8 -> Word8 -> Word16
makeWord16 w1 w2 = ((fromIntegral w1) `shiftL` 8) .|. (fromIntegral w2)

-- | Split the 2-bytes into pieces, the most significant bits being the first 
-- of the pair.
splitWord16 :: Word16 -> (Word8, Word8)
splitWord16 w = ((fromIntegral w) `shiftR` 8, fromIntegral w)

-- | Decrement the value to 0, unless it is already <= 0.
decrementToZero :: Word8 -> Word8
decrementToZero word 
    | word > 0 = word - 1
    | otherwise = 0

-- | Add the two bytes together, also returning a 1 if there was a carry bit 
-- or 0 otherwise.
addCarry :: Word8 -> Word8 -> (Word8, Word8)
addCarry n m 
    | (n .&. m .&. 0x80) == 0x80 = (n + m, 1)
    | otherwise = (n + m, 0)

-- | Subtract the second byte from the first, also returning a 0 if there was 
-- a borrow or 1 otherwise.
subtractCarry :: Word8 -> Word8 -> (Word8, Word8)
subtractCarry n m
    | n < m = (n - m, 0)
    | otherwise = (n - m, 1)

