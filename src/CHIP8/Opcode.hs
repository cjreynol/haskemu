{-|
Module      : Opcode
Description : Opcode decoding and execution
Copyright   : (c) Chad Reynolds, 2018-2023
License     : MIT
-}
{-# LANGUAGE RecordWildCards #-}

module Opcode
  ( Opcode(..)
  , OpcodeComponents(..)
  , createOpcodeComponents
  , decodeOpcode
  , getOpcode
  ) where

import           Data.Bits ((.&.), shiftR)
import           Data.Word (Word16, Word8)
import           Numeric   (showHex)

import           Util      (makeWord16)

-- | The components of the opcode, to be matched on to determine which operation to run
data OpcodeComponents
  = OpcodeComponents
    { high    :: Word8
    , low     :: Word8
    , nibble1 :: Int
    , nibble2 :: Int
    , nibble3 :: Int
    , nibble4 :: Int
    , address :: Word16 -- ^ the last 12 bits of the opcode, a memory address
    }
  deriving (Eq)

instance Show OpcodeComponents where
  show OpcodeComponents {..} = "OpcodeComponents {high = 0x" ++ showHex high ", low = 0x"
    ++ showHex low ", nibble1 = 0x" ++ showHex nibble1 ", nibble2 = 0x" ++ showHex nibble2 ", nibble3 = 0x"
    ++ showHex nibble3 ", nibble4 = 0x" ++ showHex nibble4 ", address = 0x" ++ showHex address "}"

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
data Opcode
  =
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
decodeOpcode code@(OpcodeComponents {..}) = case nibble1 of
  0x0 -> case address of
    0x0E0 -> ClearDisplay
    0x0EE -> Return
    _     -> CallRoutine address
  0x1 -> Jump address
  0x2 -> Subroutine address
  0x3 -> SkipEq nibble2 low
  0x4 -> SkipNotEq nibble2 low
  0x5 -> SkipRegEq nibble2 nibble3
  0x6 -> Assign nibble2 low
  0x7 -> AddValue nibble2 low
  0x8 -> case nibble4 of
    0x0 -> AssignReg nibble2 nibble3
    0x1 -> Or nibble2 nibble3
    0x2 -> And nibble2 nibble3
    0x3 -> Xor nibble2 nibble3
    0x4 -> Add nibble2 nibble3
    0x5 -> Subtract nibble2 nibble3
    0x6 -> ShiftR1 nibble2
    0x7 -> SubtractFlip nibble2 nibble2
    0xE -> ShiftL1 nibble2
    _   -> error $ "Unexpected value in nibble2(0x" ++ showHex nibble2 ")\n" ++ show code
  0x9 -> SkipRegNotEq nibble2 nibble3
  0xA -> SetIndex address
  0xB -> JumpAdd address
  0xC -> SetRandom nibble2 low
  0xD -> Draw nibble2 nibble3 nibble4
  0xE -> case low of
    0x009E -> SkipKey nibble2
    0x00A1 -> SkipNotKey nibble2
    _      -> error $ "Unexpected value in low(0x" ++ showHex low ")\n" ++ show code
  0xF -> case low of
    0x07 -> GetTimer nibble2
    0x0A -> GetKey nibble2
    0x15 -> SetTimer nibble2
    0x18 -> SetSound nibble2
    0x1E -> AddIndexReg nibble2
    0x29 -> SetIndexSprite nibble2
    0x33 -> BCD nibble2
    0x55 -> RegDump nibble2
    0x65 -> RegLoad nibble2
    _    -> error $ "Unexpected value in low(0x" ++ showHex low ")\n" ++ show code
  _   -> error $ "Unexpected value in nibble1(0x" ++ showHex nibble1 ")\n" ++ show code

getOpcode :: Word8 -> Word8 -> Opcode
getOpcode h l = decodeOpcode $ createOpcodeComponents h l
