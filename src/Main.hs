{-|
Module      : Main
Description : The main program
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
import Data.Word                        (Word8, Word16, Word64)


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

type Screen = IO (MVector (PrimState IO) Word64)
makeScreen :: Screen    -- 32 rows of 64 bits
makeScreen = MV.new 32

data ProgramState = ProgramState {
      registers         :: Registers
    , memory            :: Memory
    , screen            :: Screen
    -- need some representation for keypresses, maybe MVar so it blocks?
    , indexPointer      :: Word16
    , programCounter    :: Word16
    , stack             :: [Word16]
    , stackPointer      :: Word16
    , delayTimer        :: Word16
    , soundTimer        :: Word16
    }

-- TODO: implement actions 
decodeOpcodes :: Word16 -> String
decodeOpcodes code = 
    case nib1 of 
        0x0000 -> case addr of 
                    0x00E0  -> "clear screen"
                    0x00EE  -> "return"
                    _       -> "call program at addr"
        0x0001 -> "jump to addr"
        0x0002 -> "call subroutine at addr"
        0x0003 -> "skip if Reg[nib2] == low"
        0x0004 -> "skip if Reg[nib2] != low"
        0x0005 -> "skip if Reg[nib2] == Reg[nib3]"
        0x0006 -> "Reg[nib2] = low"
        0x0007 -> "Reg[nib2] += low"    -- no carry flag set
        0x0008 -> case nib4 of
                    0x0000 -> "Reg[nib2] = Reg[nib3]"
                    0x0001 -> "Reg[nib2] | Reg[nib3]"
                    0x0002 -> "Reg[nib2] & Reg[nib3]"
                    0x0003 -> "Reg[nib2] ^ Reg[nib3]"
                    0x0004 -> "Reg[nib2] += Reg[nib3];\
                                \ Reg[0xF] = if borrow then 0 else 1"
                    0x0005 -> "Reg[nib2] -= Reg[nib3];\
                                \  Reg[0xF] = if borrow then 0 else 1"
                    -- shiftR a bit out of Reg[nib2] and "into" Reg[0xF]
                    0x0006 -> "Reg[0xF] = Reg[nib2] & 0x000E;\
                                \ Reg[nib2] = Reg[nib2] `shiftR` 1"
                    0x0007 -> "Reg[nib2] = Reg[nib3] - Reg[nib2];\
                                \ Reg[0xF] = if borrow then 0 else 1"
                    -- shiftL a bit out of Reb[nib2] and "into Reg[0xF]
                    0x000E -> "Reg[0xF] = Reg[nib2] & 0x7000;\
                                \ Reg[nib2] = Reg[nib2] `shiftL` 1"
        0x0009 -> "skip if Reg[nib2] != Reg[nib3]"
        0x000A -> "I = addr"
        0x000B -> "PC = addr + Reg[0]" -- jump to addr + Reg[0]
        0x000C -> "Reg[nib2] = rand in [0,255] & low"
        0x000D -> "Draw sprite at pos (Reg[nib2], Reg[nib3])\
                    \ with width=8px height=nib4;\
                    \ each row[0-nib4] read 8 bits at a time from Mem[I];\
                    \ Reg[0xF] = 1 if any screen pixel goes from set to unset"
        0x000E -> case low of
                    0x009E -> "skip next if keypress == Reg[nib2]"
                    0x00A1 -> "skip next if keypress != Reg[nib2]"
        0x000F -> case low of
                    0x0007 -> "Reg[nib2] = delayTimer"
                    0x000A -> "Reg[nib2] = next keypress" -- block until
                    0x0015 -> "delayTimer = Reg[nib2]"
                    0x0018 -> "soundTimer = Reg[nib2]"
                    0x001E -> "I += Reg[nib2]"
                    0x0029 -> "I = spriteLocation Reg[nib2]"
                    0x0033 -> "(huns, tens, ones) = BCD(Reg[nib2];\
                                \ Mem[I] = huns;\
                                \ Mem[I+1] = tens;\
                                \ Mem[I+2] = ones"
                    -- dump registers into memory
                    0x0055 -> "Mem[I] = [Reg[0], Reg[1], ..., Reg[nib2]]"
                    -- load memory into registers
                    0x0065 -> "[Reg[0], Reg[1], ..., Reg[nib2] = Mem[I]"
        _ -> "Unexpected opcode."
    where 
        nib1 = shiftR (code .&. 0xF000) 24
        nib2 = shiftR (code .&. 0x0F00) 16
        nib3 = shiftR (code .&. 0x00F0) 8
        nib4 = code .&. 0x000F
        addr = code .&. 0x0FFF
        low  = code .&. 0x00FF

