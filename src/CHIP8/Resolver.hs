{-|
Module      : Resolver
Description : Resolves opcode actions through program state updates
Copyright   : (c) Chad Reynolds, 2023
License     : MIT
-}
{-# LANGUAGE RecordWildCards #-}

module Resolver
  ( resolveOpcode
  ) where

import           Data.Bits           ((.&.), (.|.), shiftL, shiftR, xor)
import           Data.Word           (Word16, Word8)
import           Data.Vector         as V ((!), elemIndex, freeze, fromList, slice, thaw, zip)
import           Data.Vector.Mutable as MV (IOVector, modify, read, write)

import           System.Random       (getStdRandom, randomR)

import           Opcode
  ( Opcode(ClearDisplay, Draw, Return, Jump, JumpAdd, Subroutine,
       SkipEq, SkipNotEq, SkipRegEq, SkipRegNotEq, Assign, AddValue,
       AssignReg, SetRandom, Or, And, Xor, ShiftR1, ShiftL1, Add,
       Subtract, SubtractFlip, SkipKey, SkipNotKey, GetKey, GetTimer,
       SetTimer, SetSound, SetIndex, AddIndexReg, SetIndexSprite, RegDump,
       RegLoad, BCD, CallRoutine)
  )
import           ProgramState
  ( ProgramState(ProgramState, registers, memory, screen,
             indexRegister, programCounter, delayTimer, keyState,
             screenModified)
  , addToIndex
  , callSubroutine
  , clearScreen
  , returnFromSubroutine
  , setDelay
  , setIndex
  , setIndexToFont
  , setProgramCounter
  , setProgramCounterPlusReg
  , setSound
  , skipIfEq
  , skipIfKey
  , skipIfNotEq
  , skipIfNotKey
  , skipIfRegEq
  , skipIfRegNotEq
  )
import           Util                (addCarry, makeWord16, splitWord16, subtractCarry)

-- | Take the appropriate action on the ProgramState according to the given opcode
resolveOpcode :: Opcode -> ProgramState -> IO ProgramState
resolveOpcode ClearDisplay = pure . clearScreen
resolveOpcode (Draw rX rY n) = drawSprite rX rY n
resolveOpcode Return = pure . returnFromSubroutine
resolveOpcode (Jump addr) = pure . setProgramCounter addr
resolveOpcode (JumpAdd addr) = pure . setProgramCounterPlusReg addr
resolveOpcode (Subroutine addr) = pure . callSubroutine addr
resolveOpcode (SkipEq r n) = pure . skipIfEq r n
resolveOpcode (SkipNotEq r n) = pure . skipIfNotEq r n
resolveOpcode (SkipRegEq rX rY) = pure . skipIfRegEq rX rY
resolveOpcode (SkipRegNotEq rX rY) = pure . skipIfRegNotEq rX rY
resolveOpcode (Assign r n) = setRegVal r n
resolveOpcode (AddValue r n) = addRegVal r n
resolveOpcode (AssignReg rX rY) = setReg rX rY
resolveOpcode (SetRandom r n) = randGen r n
resolveOpcode (Or rX rY) = setRegOp (.|.) rX rY
resolveOpcode (And rX rY) = setRegOp (.&.) rX rY
resolveOpcode (Xor rX rY) = setRegOp xor rX rY
resolveOpcode (ShiftR1 r) = shiftOutReg shiftR r 0x01
resolveOpcode (ShiftL1 r) = shiftOutReg shiftL r 0x80
resolveOpcode (Add rX rY) = setRegOpCarry addCarry rX rY
resolveOpcode (Subtract rX rY) = setRegOpCarry subtractCarry rX rY
resolveOpcode (SubtractFlip rX rY) = setRegOpCarry2 subtractCarry rX rY
resolveOpcode (SkipKey r) = pure . skipIfKey r
resolveOpcode (SkipNotKey r) = pure . skipIfNotKey r
resolveOpcode (GetKey r) = getNextKey r
resolveOpcode (GetTimer r) = getDelay r
resolveOpcode (SetTimer r) = pure . setDelay r
resolveOpcode (SetSound r) = pure . setSound r
resolveOpcode (SetIndex addr) = pure . setIndex addr
resolveOpcode (AddIndexReg r) = pure . addToIndex r
resolveOpcode (SetIndexSprite r) = pure . setIndexToFont r
resolveOpcode (RegDump r) = dumpFromRegs r
resolveOpcode (RegLoad r) = loadFromMem r
resolveOpcode (BCD r) = binCD r
resolveOpcode (CallRoutine _) = pure

-- | Set the register to the given value.
setRegVal :: Int -> Word8 -> ProgramState -> IO ProgramState
setRegVal i val pState = do
  regs <- thaw $ registers pState
  write regs i val
  newRegs <- freeze regs
  pure $ pState { registers = newRegs
                }

-- | Add the given value to the current value of the register.
-- Note:  Does not set the carry flag.
addRegVal :: Int -> Word8 -> ProgramState -> IO ProgramState
addRegVal i val pState = do
  regs <- thaw $ registers pState
  MV.modify regs (+ val) i
  newRegs <- freeze regs
  pure $ pState { registers = newRegs
                }

-- | Set the first register to the value of the second.
setReg :: Int -> Int -> ProgramState -> IO ProgramState
setReg i1 i2 pState = do
  regs <- thaw $ registers pState
  MV.read regs i2 >>= write regs i1
  newRegs <- freeze regs
  pure $ pState { registers = newRegs
                }

-- | Set the first register to the value of the function with the value of 
-- both registers as arguments.
setRegOp :: (Word8 -> Word8 -> Word8) -> Int -> Int -> ProgramState -> IO ProgramState
setRegOp op i1 i2 pState = do
  regs <- thaw $ registers pState
  r1Val <- MV.read regs i1
  r2Val <- MV.read regs i2
  write regs i1 $ r1Val `op` r2Val
  newRegs <- freeze regs
  pure $ pState { registers = newRegs
                }

-- | Set the first register to the value of the function with the value of 
-- both of the registers as arguments.  Also, set the carry flag based on the 
-- secondary result of the function.
setRegOpCarry :: (Word8 -> Word8 -> (Word8, Word8)) -> Int -> Int -> ProgramState -> IO ProgramState
setRegOpCarry op i1 i2 pState = do
  regs <- thaw $ registers pState
  r1Val <- MV.read regs i1
  r2Val <- MV.read regs i2
  let (result, carry) = r1Val `op` r2Val
  write regs i1 result
  write regs 0xF carry
  newRegs <- freeze regs
  pure $ pState { registers = newRegs
                }

-- | The same as setRegOpCarry except the order of the parameters is reversed.
setRegOpCarry2 :: (Word8 -> Word8 -> (Word8, Word8)) -> Int -> Int -> ProgramState -> IO ProgramState
setRegOpCarry2 op i1 i2 pState = do
  regs <- thaw $ registers pState
  r1Val <- MV.read regs i1
  r2Val <- MV.read regs i2
  let (result, carry) = r2Val `op` r1Val
  write regs i1 result
  write regs 0xF carry
  newRegs <- freeze regs
  pure $ pState { registers = newRegs
                }

-- | Shift the given register using the given function, and put that bit into 
-- the carry register.
shiftOutReg :: (Word8 -> Int -> Word8) -> Int -> Word8 -> ProgramState -> IO ProgramState
shiftOutReg op i andVal pState = do
  regs <- thaw $ registers pState
  val <- MV.read regs i
  write regs 0xF $ val .&. andVal
  write regs i $ val `op` 1
  newRegs <- freeze regs
  pure $ pState { registers = newRegs
                }

--"Reg[nib2] = rand in [0,255] & low"
-- | Set the given register to a random number [0, 255] ANDed with the given 
-- value.
randGen :: Int -> Word8 -> ProgramState -> IO ProgramState
randGen i val pState = do
  regs <- thaw $ registers pState
  randNum <- getStdRandom $ randomR (0x00, 0xFF)
  write regs i $ randNum .&. val
  newRegs <- freeze regs
  pure $ pState { registers = newRegs
                }

-- | Draw a sprite at the screen location indexed by the two given register 
-- values, it is 8-bits wide and byteNum rows tall.  The sprite is located in 
-- memory at the address in index register, and consists of the given byteNum 
-- number of bytes.
-- Note;  The carry flag is set to 1 if any screen pixel goes from set to 
-- unset, 0 otherwise.  This is used for collision detection.
drawSprite :: Int -> Int -> Int -> ProgramState -> IO ProgramState
drawSprite i1 i2 byteNum p@ProgramState {..} = do
  scr <- thaw screen
  updates <- mapM (`helper` scr) [0 .. byteNum]
  let pixelUpdate = if foldr (.|.) 0 updates > 0 then 1 else 0
  regs <- thaw registers
  write regs 0xF pixelUpdate
  newRegs <- freeze regs
  newScreen <- freeze scr
  pure $ p { screen         = newScreen
           , registers      = newRegs
           , screenModified = True
           }
  where
    index = fromIntegral indexRegister
    spriteData = slice index (8 * byteNum) memory
    x = fromIntegral $ registers ! i1
    y = fromIntegral $ registers ! i2
    screenIndex = (y * 8) + (x `div` 8)
    b1LShift = 16 - (x `rem` 8)
    helper :: Int -> IOVector Word8 -> IO Word16
    helper i mScreen = do
      b1 <- MV.read mScreen $ screenIndex + (i * 8)
      b2 <- MV.read mScreen $ screenIndex + (i * 8) + 1
      let curWord    = makeWord16 b1 b2
          newData    = fromIntegral (spriteData ! i) `shiftL` b1LShift
          newWord    = curWord `xor` newData
          updated    = curWord .&. newData
          (b1', b2') = splitWord16 newWord
      write mScreen screenIndex b1'
      write mScreen screenIndex b2'
      pure updated

-- | Set the given register to the value in the delay timer.
getDelay :: Int -> ProgramState -> IO ProgramState
getDelay i pState = do
  regs <- thaw $ registers pState
  write regs i $ delayTimer pState
  newRegs <- freeze regs
  pure $ pState { registers = newRegs
                }

-- | Block the program execution until a key is pressed, then set the given 
-- register to the value of that key.
getNextKey :: Int -> ProgramState -> IO ProgramState
getNextKey i p@ProgramState {..} = case elemIndex True keyState of
  (Just keyI) -> do
    regs <- thaw registers
    write regs i $ fromIntegral keyI
    newRegs <- freeze regs
    pure $ p { registers = newRegs
             }
  Nothing     -> pure $ p { programCounter = programCounter - 2
                          }

-- | Store the binary-coded decimal representation of the number in the 
-- given register in memory where the index register is pointing to.
binCD :: Int -> ProgramState -> IO ProgramState
binCD i pState = let index = fromIntegral $ indexRegister pState
                     val   = registers pState ! i
                     ones  = val `rem` 10
                     tens  = val `div` 10 `rem` 10
                     huns  = val `div` 100 in do
  mem <- thaw $ memory pState
  write mem index huns
  write mem (index + 1) tens
  write mem (index + 2) ones
  newMem <- freeze mem
  pure $ pState { memory = newMem
                }

-- | Dump registers 0 to the given num into memory starting at the address 
-- in the index register.
dumpFromRegs :: Int -> ProgramState -> IO ProgramState
dumpFromRegs n pState = let index = fromIntegral $ indexRegister pState in do
  mem <- thaw $ memory pState
  mapM_ (helper mem index) $ V.zip (fromList [0 .. n]) (registers pState)
  newMem <- freeze mem
  pure $ pState { memory = newMem
                }
  where
    helper :: IOVector Word8 -> Int -> (Int, Word8) -> IO ()
    helper m index (i, val) = write m (index + i) val

-- | Load data from memory into the registers 0 to the given num starting at 
-- the address in the index register.
loadFromMem :: Int -> ProgramState -> IO ProgramState
loadFromMem n p@ProgramState {..} = let index   = fromIntegral indexRegister
                                        memData = V.zip (fromList [0 .. n]) (slice index n memory) in do
  regs <- thaw registers
  mapM_ (helper regs) memData
  newRegs <- freeze regs
  pure $ p { registers = newRegs
           }
  where
    helper :: IOVector Word8 -> (Int, Word8) -> IO ()
    helper r (i, val) = write r i val
