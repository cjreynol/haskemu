{-|
Module      : Opcode
Description : Opcode decoding and execution
Copyright   : (c) Chad Reynolds, 2018-2023
License     : MIT
-}

{-# LANGUAGE RecordWildCards #-}

module Opcode (
    Opcode(..)
    , OpcodeComponents(..)
    , createOpcodeComponents
    , decodeOpcode
    , getOpcode
    ) where

import Data.Bits    ((.&.), shiftR)
import Data.Word    (Word8, Word16)

import Util         (makeWord16)


-- | The components of the opcode, to be matched on to determine which operation to run
data OpcodeComponents = OpcodeComponents 
    { high      :: Word8
    , low       :: Word8
    , nibble1   :: Int
    , nibble2   :: Int
    , nibble3   :: Int
    , nibble4   :: Int
    , address   :: Word16 -- ^ the last 12 bits of the opcode, a memory address
    } deriving (Eq, Show)

createOpcodeComponents :: Word8 -> Word8 -> OpcodeComponents
createOpcodeComponents h l = OpcodeComponents h l nib1 nib2 nib3 nib4 addr
    where
        nib1 = fromIntegral ((h .&. 0xF0) `shiftR` 4) :: Int
        nib2 = fromIntegral (h .&. 0x0F) :: Int
        nib3 = fromIntegral ((l .&. 0xF0) `shiftR` 4) :: Int
        nib4 = fromIntegral (l .&. 0x0F) :: Int
        addr = makeWord16 h l .&. 0x0FFF

-- | A 12-bit memory address
type Address = Word16

-- | A 4-bit register ID
type Register = Int

-- | An 8-bit constant value
type Immediate = Word8

-- | A 4-bit constant value
type NImmediate = Int

-- | Datatype for the resolved opcode and its arguments, to be executed
data Opcode = 

    -- * Display
    -- | clear the screen
      ClearDisplay
    -- | draw 8 by N sprite at (X,Y)
    | Draw Register Register NImmediate

    -- * Program flow
    -- | return from a subroutine
    | Return
    -- | jump to address
    | Jump Address
    -- | jump to address plus value in register 0
    | JumpAdd Address
    -- | call subroutine at address
    | Subroutine Address

    -- * Conditionals
    -- | skip next instruction if value in register == immediate
    | SkipEq Register Immediate
    -- | skip next instruction if value in register != immediate
    | SkipNotEq Register Immediate
    -- | skip next instruction iv value in registers are equal
    | SkipRegEq Register Register
    -- | skip next instruction if values in registers are not equal
    | SkipRegNotEq Register Register

    -- * Assignment operations
    -- | X = NN
    | Assign Register Immediate
    -- | X += NN (carry flag not changed)
    | AddValue Register Immediate
    -- | X = Y
    | AssignReg Register Register
    -- | X = random() & NN
    | SetRandom Register Immediate

    -- Bitwise operations
    -- | X |= Y
    | Or Register Register
    -- | X &= Y
    | And Register Register
    -- | X ^= Y
    | Xor Register Register
    -- | X >>= 1
    | ShiftR1 Register
    -- | X <<= 1
    | ShiftL1 Register

    -- * Arithmetic operations
    -- | X += Y
    | Add Register Register
    -- | X -= Y
    | Subtract Register Register
    -- | X = Y - X
    | SubtractFlip Register Register

    -- * Key operations
    -- | skip next instruction if key stored in X is pressed
    | SkipKey Register
    -- | skip next instruction if key stored in X is not pressed
    | SkipNotKey Register
    -- | X = get_key()
    | GetKey Register
    
    -- * Timer operations
    -- | X = get_delay_timer()
    | GetTimer Register
    -- | set_delay_timer(X)
    | SetTimer Register
    -- | set_sound_timer(X)
    | SetSound Register

    -- * Memory operations
    -- | set address register to address
    | SetIndex Address
    -- | set address register to X
    | AddIndexReg Register
    -- | set address register to the location of the sprite for character in X
    | SetIndexSprite Register
    -- | register dump 0 to X to memory
    | RegDump Register
    -- | register load 0 to X from memory
    | RegLoad Register

    -- * Other
    -- | set_binary_coded_decimal(X)
    | BCD Register
    -- ** Unused
    -- | call machine code routine at address
    | CallRoutine Address
  deriving (Show)

-- | Decode the opcode according to the CHIP-8 specification, gathering arguments for resolving it
decodeOpcode :: OpcodeComponents -> Opcode
decodeOpcode code@(OpcodeComponents{..}) =
    case nibble1 of 
        0x0000 -> case address of 
                    0x00E0  -> ClearDisplay
                    0x00EE  -> Return
                    _       -> CallRoutine address
        0x0001 -> Jump address
        0x0002 -> Subroutine address
        0x0003 -> SkipEq nibble2 low
        0x0004 -> SkipNotEq nibble2 low
        0x0005 -> SkipRegEq nibble2 nibble3
        0x0006 -> Assign nibble2 low
        0x0007 -> AddValue nibble2 low
        0x0008 -> case nibble4 of
                    0x0000 -> AssignReg nibble2 nibble3
                    0x0001 -> Or nibble2 nibble3
                    0x0002 -> And nibble2 nibble3
                    0x0003 -> Xor nibble2 nibble3
                    0x0004 -> Add nibble2 nibble3
                    0x0005 -> Subtract nibble2 nibble3
                    0x0006 -> ShiftR1 nibble2
                    0x0007 -> SubtractFlip nibble2 nibble2
                    0x000E -> ShiftL1 nibble2
                    _       -> error $ "Unexpected opcode:  " ++ show code
        0x0009 -> SkipRegNotEq nibble2 nibble3
        0x000A -> SetIndex address
        0x000B -> JumpAdd address
        0x000C -> SetRandom nibble2 low
        0x000D -> Draw nibble2 nibble3 nibble4
        0x000E -> case low of
                    0x009E -> SkipKey nibble2
                    0x00A1 -> SkipNotKey nibble2
                    _       -> error $ "Unexpected opcode:  " ++ show code
        0x000F -> case low of
                    0x0007 -> GetTimer nibble2
                    0x000A -> GetKey nibble2
                    0x0015 -> SetTimer nibble2
                    0x0018 -> SetSound nibble2
                    0x001E -> AddIndexReg nibble2
                    0x0029 -> SetIndexSprite nibble2
                    0x0033 -> BCD nibble2
                    0x0055 -> RegDump nibble2
                    0x0065 -> RegLoad nibble2
                    _       -> error $ "Unexpected opcode:  " ++ show code
        _ -> error $ "Unexpected opcode:  " ++ show code

getOpcode :: Word8 -> Word8 -> Opcode
getOpcode h l = decodeOpcode $ createOpcodeComponents h l
