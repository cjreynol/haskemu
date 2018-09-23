{-|
Module      : CHIP8.Opcode
Description : Opcode decoding and execution
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}

module CHIP8.Opcode (
    handleOpcode
    ) where

import Data.Bits            ((.&.), shiftR)
import Data.Word            (Word16)

import CHIP8.ProgramState   (ProgramState)


-- | Decode the opcode according to the CHIP-8 specification and take the 
-- appropriate action on the ProgramState.
handleOpcode :: Word16 -> ProgramState -> String
handleOpcode code pState = 
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
                    _       -> "Unexpected opcode:  " ++ (show code)
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
                    _       -> "Unexpected opcode:  " ++ (show code)
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
                    _       -> "Unexpected opcode:  " ++ (show code)
        _ -> "Unexpected opcode:  " ++ (show code)
    where 
        nib1 = shiftR (code .&. 0xF000) 24  -- first 4 bits of 1st byte
        nib2 = shiftR (code .&. 0x0F00) 16  -- second 4 bits of 1st byte
        nib3 = shiftR (code .&. 0x00F0) 8   -- first 4 bits of 2nd byte
        nib4 = code .&. 0x000F              -- second 4 bits of 2nd byte
        addr = code .&. 0x0FFF              -- last 12 bits of 2 bytes
        low  = code .&. 0x00FF              -- 2nd byte

