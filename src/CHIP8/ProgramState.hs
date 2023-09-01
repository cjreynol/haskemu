{-|
Module      : ProgramState
Description : The state of the program including registers, memory, etc
Copyright   : (c) Chad Reynolds, 2018-2023
License     : MIT
-}
module ProgramState
  ( KeyState
  , Memory
  , ProgramData
  , Registers
  , Screen
  , ProgramState(..)
  , fontDataAddr
  , initializeProgram
  ) where

import           Data.Vector as V (Vector, concat, fromList, length, replicate)
import           Data.Word   (Word16, Word8)

type ProgramData = Vector Word8

type Registers = Vector Word8

type Memory = Vector Word8

type Screen = Vector Word8

type KeyState = Vector Bool

-- | The datatype containing all of the information needed to run the program.
data ProgramState
  = ProgramState
    { -- | The 15 standard registers and carry register used for computation
      registers      :: Registers
      -- | The 4096 bytes of memory, which contains the font data and program 
      -- data (code and sprite data together)
    , memory         :: Memory
      -- | The representation of all of the screen pixels, 32 rows of 64 pixels
    , screen         :: Screen
      -- | The index register, twice as large as the working registers.  
      -- Needed for the program to be able to address memory.  Since the 
      -- registers are only 8-bits, they are not large enough to address the 
      -- entirety of the 4096 bytes (12-bit addresses).
    , indexRegister  :: Word16
      -- | The program counter, pointing to the current opcode byte pair in 
      -- memory.
    , programCounter :: Word16
      -- | The address stack, used to store return addresses when jumping to 
      -- subroutines.
    , stack          :: [ Word16 ]
      -- | The delay timer, runs at 60Hz.
    , delayTimer     :: Word8
      -- | The sound timer, runs at 60Hz and triggers sound effects while it is 
      -- nonzero.
    , soundTimer     :: Word8
      -- | The current state of the 16 keys, indexed [0x0-0xF] with True 
      -- indicating the key is pressed and False indicating it is not.
    , keyState       :: KeyState
      -- | Indicates whether the screen data has changed and needs to be 
      -- re-drawn.
    , screenModified :: Bool
    }

-- | The default start position of the program in memory.
programStartAddr :: Word16
programStartAddr = 0x200

-- | The start position of the font data in memory.
fontDataAddr :: Word16
fontDataAddr = 0x80

-- | Create the ProgramState with the font and program data loaded into 
-- memory, and the rest of the attributes in an initial state.
initializeProgram :: ProgramData -> ProgramState
initializeProgram pData = ProgramState makeRegisters (makeMemory pData) makeScreen 0x0 programStartAddr [] 0x0 0x0
  makeKeyState False

makeRegisters :: Registers
makeRegisters = V.replicate 16 0

makeMemory :: ProgramData -> Memory
makeMemory pData = V.concat
  [ V.replicate 80 0              -- 0x000 to 0x050
  , fontData                      -- 0x050 to 0x0A0
  , V.replicate 352 0             -- 0x0A0 to 0x200
  , pData                         -- 0x200 to ??
  , V.replicate (4096 - V.length pData) 0 -- ?? to 0x1000
  ]

makeScreen :: Screen
makeScreen = V.replicate 256 0

fontData :: Vector Word8
fontData = V.fromList
  $ zero ++ one ++ two ++ three ++ four ++ five ++ six ++ seven ++ eight ++ nine ++ a ++ b ++ c ++ d ++ e ++ f
  where
    zero  = [0xF0, 0x90, 0x90, 0x90, 0xF0]
    one   = [0x20, 0x60, 0x20, 0x20, 0x70]
    two   = [0xF0, 0x10, 0xF0, 0x80, 0xF0]
    three = [0xF0, 0x10, 0xF0, 0x10, 0xF0]
    four  = [0x90, 0x90, 0xF0, 0x10, 0x10]
    five  = [0xF0, 0x80, 0xF0, 0x10, 0xF0]
    six   = [0xF0, 0x80, 0xF0, 0x90, 0xF0]
    seven = [0xF0, 0x10, 0x20, 0x40, 0x40]
    eight = [0xF0, 0x90, 0xF0, 0x90, 0xF0]
    nine  = [0xF0, 0x90, 0xF0, 0x10, 0xF0]
    a     = [0xF0, 0x90, 0xF0, 0x90, 0x90]
    b     = [0xE0, 0x90, 0xE0, 0x90, 0xE0]
    c     = [0xF0, 0x80, 0x80, 0x80, 0xF0]
    d     = [0xE0, 0x90, 0x90, 0x90, 0xE0]
    e     = [0xF0, 0x80, 0xF0, 0x80, 0xF0]
    f     = [0xF0, 0x80, 0xF0, 0x80, 0x80]

makeKeyState :: KeyState
makeKeyState = V.replicate 16 False
