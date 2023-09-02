{-|
Module      : ProgramState
Description : The state of the program including registers, memory, etc
Copyright   : (c) Chad Reynolds, 2018-2023
License     : MIT
-}
{-# LANGUAGE RecordWildCards #-}

module ProgramState
  ( KeyState
  , Memory
  , ProgramData
  , Registers
  , Screen
  , ProgramState(..)
  , addToIndex
  , callSubroutine
  , clearScreen
  , initializeProgram
  , modifyMemory
  , modifyRegisters
  , modifyScreen
  , returnFromSubroutine
  , setDelay
  , setIndex
  , setIndexToFont
  , setProgramCounter
  , setProgramCounterPlusReg
  , setSound
  , skipIfEq
  , skipIfKey
  , skipIfNotKey
  , skipIfNotEq
  , skipIfRegEq
  , skipIfRegNotEq
  ) where

import           Control.Monad.ST (ST)
import           Data.Vector      as V ((!), MVector, Vector, concat, fromList, length, modify, replicate)
import           Data.Word        (Word16, Word8)

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

-- | Clear the screen, setting all the bits to 0.
clearScreen :: ProgramState -> ProgramState
clearScreen pState = pState { screen         = makeScreen
                            , screenModified = True
                            }

-- | Call the subroutine at the given address.
callSubroutine :: Word16 -> ProgramState -> ProgramState
callSubroutine addr p@ProgramState {..} = p
  { stack          = programCounter : stack
  , programCounter = addr
  }

-- | Return from a subroutine.
returnFromSubroutine :: ProgramState -> ProgramState
returnFromSubroutine p@ProgramState {..} = p
  { stack          = tail stack
  , programCounter = head stack
  }

-- | Jump to the given address.
setProgramCounter :: Word16 -> ProgramState -> ProgramState
setProgramCounter addr pState = pState { programCounter = addr
                                       }

-- | Jump to the address plus register 0.
setProgramCounterPlusReg :: Word16 -> ProgramState -> ProgramState
setProgramCounterPlusReg addr pState = let val = fromIntegral $ registers pState ! 0 in setProgramCounter
  (addr + val) pState

-- | Set the index register to the given address.
setIndex :: Word16 -> ProgramState -> ProgramState
setIndex addr pState = pState { indexRegister = addr
                              }

-- | Add the value in the given register to the index register.
addToIndex :: Int -> ProgramState -> ProgramState
addToIndex i p@ProgramState {..} = let val = fromIntegral $ registers ! i in p
  { indexRegister = indexRegister + val
  }

-- | Set the index register to the memory address of the image data for the 
-- digit stored in the given register.
setIndexToFont :: Int -> ProgramState -> ProgramState
setIndexToFont i pState = let char = fromIntegral $ registers pState ! i
                              addr = char * 5 + fontDataAddr in pState
  { indexRegister = addr
  }

-- | Set the delay timer to the value in the given register.
setDelay :: Int -> ProgramState -> ProgramState
setDelay i pState = let val = registers pState ! i in pState
  { delayTimer = val
  }

-- | Set the sound timer to the value in the given register.
setSound :: Int -> ProgramState -> ProgramState
setSound i pState = let val = registers pState ! i in pState
  { soundTimer = val
  }

-- | Skip the next instruction based on the condition.
skipInstruction :: Bool -> ProgramState -> ProgramState
skipInstruction True p@ProgramState {..} = p { programCounter = programCounter + 2
                                             }
skipInstruction False p = p

-- | Skip the next instruction if the value in the register is equal to the immediate value
skipIfEq :: Int -> Word8 -> ProgramState -> ProgramState
skipIfEq i val p@ProgramState {..} = skipInstruction ((registers ! i) == val) p

-- | Skip the next instruction if the value in the register is not equal to the immediate value
skipIfNotEq :: Int -> Word8 -> ProgramState -> ProgramState
skipIfNotEq i val p@ProgramState {..} = skipInstruction ((registers ! i) /= val) p

-- | Skip the next instruction if the value in the register is equal to the value in the other register
skipIfRegEq :: Int -> Int -> ProgramState -> ProgramState
skipIfRegEq i1 i2 p@ProgramState {..} = skipInstruction ((registers ! i1) == (registers ! i2)) p

-- | Skip the next instruction if the value in the register is not equal to the value in the other register
skipIfRegNotEq :: Int -> Int -> ProgramState -> ProgramState
skipIfRegNotEq i1 i2 p@ProgramState {..} = skipInstruction ((registers ! i1) /= (registers ! i2)) p

-- | Skip the next instruction if the key value in the register is pressed
skipIfKey :: Int -> ProgramState -> ProgramState
skipIfKey i p@ProgramState {..} = skipInstruction (keyState ! fromIntegral (registers ! i)) p

-- | Skip the next instruction if the key value in the register is not pressed
skipIfNotKey :: Int -> ProgramState -> ProgramState
skipIfNotKey i p@ProgramState {..} = skipInstruction (not (keyState ! fromIntegral (registers ! i))) p

modifyRegisters :: (forall s . MVector s Word8 -> ST s ()) -> ProgramState -> ProgramState
modifyRegisters f p@ProgramState {..} = p { registers = V.modify f registers
                                          }

modifyMemory :: (forall s . MVector s Word8 -> ST s ()) -> ProgramState -> ProgramState
modifyMemory f p@ProgramState {..} = p { memory = V.modify f memory
                                          }

modifyScreen :: (forall s . MVector s Word8 -> ST s ()) -> ProgramState -> ProgramState
modifyScreen f p@ProgramState {..} = p { screen = V.modify f screen
                                          }
