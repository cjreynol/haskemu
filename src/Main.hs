{-|
Module      : Main
Description : The main loop
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}

module Main (
    main
    ) where

import Control.Monad.Primitive          (PrimState)
import Data.Bits
import Data.Vector              as V
import Data.Vector.Mutable      as MV
import Data.Word                        (Word8, Word16)


-- | Initialize the program state, load the program into memory, then begin
-- execution loop.
main :: IO ()
main = do
    regs <- makeRegisters
    MV.write regs 10 0x00
    MV.write regs 11 0xAA
    fRegs <- V.freeze regs
    putStrLn $ w16ToStr (makeWord16 (fRegs V.! 10) (fRegs V.! 11))

w16ToStr :: Word16 -> String
w16ToStr (0x00AA) = "matched"
w16ToStr n      = "unmatched" Prelude.++ show n

makeWord16 :: Word8 -> Word8 -> Word16
makeWord16 w1 w2 = ((fromIntegral w1) `shiftL` 8) .|. (fromIntegral w2)


type Registers = IO (MVector (PrimState IO) Word8)
makeRegisters :: Registers
makeRegisters = MV.new 16

type Memory = IO (MVector (PrimState IO) Word8)
makeMemory :: Memory
makeMemory = MV.new 4096

data ProgramState = ProgramState {
      registers         :: Registers
    , memory            :: Memory
    , indexReg          :: Word16
    , programCounter    :: Word16
    , stack             :: [Word16]
    , stackPointer      :: Word16
    , delayTimer        :: Word16
    , soundTimer        :: Word16
    }


{-
TODO:
-implement 35 opcodes (16 bits each)            -Word16 -> actions
    -pattern match
    -take actions
-screen 64x32 bits                              -MVector Word64?
-}

