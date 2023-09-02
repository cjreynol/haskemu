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
import           Data.Word           (Word8)
import qualified Data.Vector         as V ((!), elemIndex, fromList, slice, zip, zipWith)
import qualified Data.Vector.Mutable as MV (modify, read, write)

import           System.Random       (genWord8, mkStdGen)

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
  , skipIfNotEq
  , skipIfNotKey
  , skipIfRegEq
  , skipIfRegNotEq
  )
import           Util                (addCarry, makeWord16, splitWord16, subtractCarry)

-- | Take the appropriate action on the ProgramState according to the given opcode
resolveOpcode :: Opcode -> ProgramState -> ProgramState
resolveOpcode ClearDisplay = clearScreen
resolveOpcode (Draw rX rY n) = drawSprite rX rY n
resolveOpcode Return = returnFromSubroutine
resolveOpcode (Jump addr) = setProgramCounter addr
resolveOpcode (JumpAdd addr) = setProgramCounterPlusReg addr
resolveOpcode (Subroutine addr) = callSubroutine addr
resolveOpcode (SkipEq r n) = skipIfEq r n
resolveOpcode (SkipNotEq r n) = skipIfNotEq r n
resolveOpcode (SkipRegEq rX rY) = skipIfRegEq rX rY
resolveOpcode (SkipRegNotEq rX rY) = skipIfRegNotEq rX rY
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
resolveOpcode (SkipKey r) = skipIfKey r
resolveOpcode (SkipNotKey r) = skipIfNotKey r
resolveOpcode (GetKey r) = getNextKey r
resolveOpcode (GetTimer r) = getDelay r
resolveOpcode (SetTimer r) = setDelay r
resolveOpcode (SetSound r) = setSound r
resolveOpcode (SetIndex addr) = setIndex addr
resolveOpcode (AddIndexReg r) = addToIndex r
resolveOpcode (SetIndexSprite r) = setIndexToFont r
resolveOpcode (RegDump r) = dumpFromRegs r
resolveOpcode (RegLoad r) = loadFromMem r
resolveOpcode (BCD r) = binCD r
resolveOpcode (CallRoutine _) = id -- unused, not expecting to encounter it

-- | Set the register to the given value.
setRegVal :: Int -> Word8 -> ProgramState -> ProgramState
setRegVal i val = modifyRegisters (\v -> MV.write v i val)

-- | Add the given value to the current value of the register.
-- Note:  Does not set the carry flag.
addRegVal :: Int -> Word8 -> ProgramState -> ProgramState
addRegVal i val = modifyRegisters (\v -> MV.modify v (+ val) i)

-- | Set the first register to the value of the second.
setReg :: Int -> Int -> ProgramState -> ProgramState
setReg i1 i2 = modifyRegisters (\v -> MV.read v i2 >>= MV.write v i1)

-- | Set the first register to the value of the function with the value of 
-- both registers as arguments.
setRegOp :: (Word8 -> Word8 -> Word8) -> Int -> Int -> ProgramState -> ProgramState
setRegOp op i1 i2 = modifyRegisters
  (\v -> do
     r1Val <- MV.read v i1
     r2Val <- MV.read v i2
     MV.write v i1 $ r1Val `op` r2Val)

-- | Set the first register to the value of the function with the value of 
-- both of the registers as arguments.  Also, set the carry flag based on the 
-- secondary result of the function.
setRegOpCarry :: (Word8 -> Word8 -> (Word8, Word8)) -> Int -> Int -> ProgramState -> ProgramState
setRegOpCarry op i1 i2 = modifyRegisters
  (\v -> do
     r1Val <- MV.read v i1
     r2Val <- MV.read v i2
     let (result, carry) = r1Val `op` r2Val
     MV.write v i1 result
     MV.write v 0xF carry)

-- | The same as setRegOpCarry except the order of the parameters is reversed.
setRegOpCarry2 :: (Word8 -> Word8 -> (Word8, Word8)) -> Int -> Int -> ProgramState -> ProgramState
setRegOpCarry2 op i1 i2 = modifyRegisters
  (\v -> do
     r1Val <- MV.read v i1
     r2Val <- MV.read v i2
     let (result, carry) = r2Val `op` r1Val
     MV.write v i1 result
     MV.write v 0xF carry)

-- | Shift the given register using the given function, and put that bit into 
-- the carry register.
shiftOutReg :: (Word8 -> Int -> Word8) -> Int -> Word8 -> ProgramState -> ProgramState
shiftOutReg op i andVal = modifyRegisters
  (\v -> do
     val <- MV.read v i
     MV.write v 0xF $ val .&. andVal
     MV.write v i $ val `op` 1)

--"Reg[nib2] = rand in [0,255] & low"
-- | Set the given register to a random number [0, 255] ANDed with the given 
-- value.
-- TODO CJR: completely deterministic, come back and revist a model for actually injecting randomness
--  in the future
randGen :: Int -> Word8 -> ProgramState -> ProgramState
randGen i val = modifyRegisters (\v -> let (randNum, _) = genWord8 $ mkStdGen 0 in MV.write v i $ randNum .&. val)

-- | Draw a sprite at the screen location indexed by the two given register 
-- values, it is 8-bits wide and byteNum rows tall.  The sprite is located in 
-- memory at the address in index register, and consists of the given byteNum 
-- number of bytes.
-- Note;  The carry flag is set to 1 if any screen pixel goes from set to 
-- unset, 0 otherwise.  This is used for collision detection.
drawSprite :: Int -> Int -> Int -> ProgramState -> ProgramState
drawSprite i1 i2 byteNum pState = do
  let screenCopy  = screen pState
      pState'     = modifyScreen (\v -> mapM_ (`helper` v) [0 .. byteNum]) pState
      pixelUpdate = foldr (.|.) 0x0 $ V.zipWith (\elem1 elem2 -> if elem1 == elem2 then 0x0 else 0x1) screenCopy
        (screen pState')
      p''         = modifyRegisters (\v -> MV.write v 0xF pixelUpdate) pState'
  p'' { screenModified = True
      }
  where
    index = fromIntegral $ indexRegister pState
    spriteData = V.slice index (8 * byteNum) $ memory pState
    x = fromIntegral $ registers pState V.! i1
    y = fromIntegral $ registers pState V.! i2
    screenIndex = (y * 8) + (x `div` 8)
    b1LShift = 16 - (x `rem` 8)
    helper i mScreen = do
      b1 <- MV.read mScreen $ screenIndex + (i * 8)
      b2 <- MV.read mScreen $ screenIndex + (i * 8) + 1
      let curWord    = makeWord16 b1 b2
          newData    = fromIntegral (spriteData V.! i) `shiftL` b1LShift
          newWord    = curWord `xor` newData
          (b1', b2') = splitWord16 newWord
      MV.write mScreen screenIndex b1'
      MV.write mScreen screenIndex b2'

-- | Set the given register to the value in the delay timer.
getDelay :: Int -> ProgramState -> ProgramState
getDelay i pState = modifyRegisters (\v -> MV.write v i $ delayTimer pState) pState

-- | Block the program execution until a key is pressed, then set the given 
-- register to the value of that key.
getNextKey :: Int -> ProgramState -> ProgramState
getNextKey i p@ProgramState {..} = case V.elemIndex True keyState of
  (Just keyI) -> modifyRegisters (\v -> MV.write v i $ fromIntegral keyI) p
  Nothing     -> p { programCounter = programCounter - 2
                   }

-- | Store the binary-coded decimal representation of the number in the 
-- given register in memory where the index register is pointing to.
binCD :: Int -> ProgramState -> ProgramState
binCD i pState = let index = fromIntegral $ indexRegister pState
                     val   = registers pState V.! i
                     ones  = val `rem` 10
                     tens  = val `div` 10 `rem` 10
                     huns  = val `div` 100 in modifyMemory
  (\v -> do
     MV.write v index huns
     MV.write v (index + 1) tens
     MV.write v (index + 2) ones) pState

-- | Dump registers 0 to the given num into memory starting at the address 
-- in the index register.
dumpFromRegs :: Int -> ProgramState -> ProgramState
dumpFromRegs n pState = let index = fromIntegral $ indexRegister pState in modifyMemory
  (\v -> do
     mapM_ (\(i, val) -> MV.write v (index + i) val) $ V.zip (V.fromList [0 .. n]) (registers pState)) pState

-- | Load data from memory into the registers 0 to the given num starting at 
-- the address in the index register.
loadFromMem :: Int -> ProgramState -> ProgramState
loadFromMem n p@ProgramState {..} = let index   = fromIntegral indexRegister
                                        memData = V.zip (V.fromList [0 .. n]) (V.slice index n memory)
  in  modifyRegisters (\v -> do
                         mapM_ (uncurry (MV.write v)) memData) p
