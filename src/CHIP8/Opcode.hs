{-|
Module      : CHIP8.Opcode
Description : Opcode decoding and execution
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}

{-# LANGUAGE RecordWildCards #-}

module CHIP8.Opcode (
    decodeOpcode
    ) where

import Data.Bits                    ((.&.), (.|.), shiftL, shiftR, xor)
import Data.Word                    (Word8, Word16)
import Data.Vector                  ((!), elemIndex, freeze, thaw)
import Data.Vector.Mutable  as MV   (read, set, write)

import System.Random                (getStdRandom, randomR)

import CHIP8.ProgramState           (ProgramState(..))
import CHIP8.Util                   (addCarry, subtractCarry)


-- | Decode the opcode according to the CHIP-8 specification and take the 
-- appropriate action on the ProgramState.
decodeOpcode :: Word16 -> ProgramState -> IO ProgramState
decodeOpcode code =
    case nib1 of 
        0x0000 -> case addr of 
                    0x00E0  -> clearScreen
                    0x00EE  -> returnFrom
                    _       -> noOp --"call program at addr"
        0x0001 -> jumpToAddr addr
        0x0002 -> callSubroutine addr
        0x0003 -> skipIfVal (==) (fromIntegral nib2) (fromIntegral low)
        0x0004 -> skipIfVal (/=) (fromIntegral nib2) (fromIntegral low)
        0x0005 -> skipIfReg (==) (fromIntegral nib2) (fromIntegral nib3)
        0x0006 -> setRegVal (fromIntegral nib2) (fromIntegral low)
        0x0007 -> addRegVal (fromIntegral nib2) (fromIntegral low)
        0x0008 -> case nib4 of
                    0x0000 -> setReg (fromIntegral nib2) (fromIntegral nib3)
                    0x0001 -> setRegOp (.|.) (fromIntegral nib2) 
                                                (fromIntegral nib3)
                    0x0002 -> setRegOp (.&.) (fromIntegral nib2)
                                                (fromIntegral nib3)
                    0x0003 -> setRegOp (xor) (fromIntegral nib2)
                                                (fromIntegral nib3)
                    0x0004 -> setRegOpCarry addCarry (fromIntegral nib2) 
                                                        (fromIntegral nib3)
                    0x0005 -> setRegOpCarry subtractCarry (fromIntegral nib2)
                                                            (fromIntegral nib3)
                    0x0006 -> shiftOutReg shiftR 0x01 (fromIntegral nib2)
                    0x0007 -> setRegOpCarry2 subtractCarry (fromIntegral nib2)
                                                            (fromIntegral nib2)
                    0x000E -> shiftOutReg shiftL 0x80 (fromIntegral nib2)
                    _       -> error $ "Unexpected opcode:  " ++ (show code)
        0x0009 -> skipIfReg (/=) (fromIntegral nib2) (fromIntegral nib3)
        0x000A -> setIndex addr
        0x000B -> jumpToAddrReg addr
        0x000C -> randGen (fromIntegral nib2) (fromIntegral low)
        0x000D -> drawSprite (fromIntegral nib2) (fromIntegral nib3) 
                                                    (fromIntegral nib4)
        0x000E -> case low of
                    0x009E -> skipIfKey (fromIntegral nib2) True
                    0x00A1 -> skipIfKey (fromIntegral nib2) False
                    _       -> error $ "Unexpected opcode:  " ++ (show code)
        0x000F -> case low of
                    0x0007 -> getDelay (fromIntegral nib2)
                    0x000A -> getNextKey (fromIntegral nib2)
                    0x0015 -> setDelay (fromIntegral nib2)
                    0x0018 -> setSound (fromIntegral nib2)
                    0x001E -> addToIndex (fromIntegral nib2)
                    0x0029 -> noOp --"I = spriteLocation Reg[nib2]"
                    0x0033 -> noOp --"(huns, tens, ones) = BCD(Reg[nib2];\
                                -- \ Mem[I] = huns;\
                                -- \ Mem[I+1] = tens;\
                                -- \ Mem[I+2] = ones"
                    -- dump registers into memory
                    0x0055 -> noOp --"Mem[I] = [Reg[0], Reg[1], ..., Reg[nib2]]"
                    -- load memory into registers
                    0x0065 -> noOp --"[Reg[0], Reg[1], ..., Reg[nib2] = Mem[I]"
                    _       -> error $ "Unexpected opcode:  " ++ (show code)
        _ -> error $ "Unexpected opcode:  " ++ (show code)
    where 
        nib1 = shiftR (code .&. 0xF000) 24  -- first 4 bits of 1st byte
        nib2 = shiftR (code .&. 0x0F00) 16  -- second 4 bits of 1st byte
        nib3 = shiftR (code .&. 0x00F0) 8   -- first 4 bits of 2nd byte
        nib4 = code .&. 0x000F              -- second 4 bits of 2nd byte
        addr = code .&. 0x0FFF              -- last 12 bits of 2 bytes
        low  = code .&. 0x00FF              -- 2nd byte

-- | Do nothing and return the state unchanged.  Used as a placeholder until 
-- opcodes are implemented.
noOp :: ProgramState -> IO ProgramState
noOp = return

-- | Clear the screen, setting all the bits to 0.
-- ????
-- Want to test a non-thaw version of this function
clearScreen :: ProgramState -> IO ProgramState
clearScreen pState = do
    curScreen <- thaw $ screen pState
    set curScreen 0
    newScreen <- freeze curScreen
    return $ pState { screen = newScreen }

-- | Return from a subroutine.
-- ???? 
-- Is the address going to be off by 2 bytes, or do programs expect it?
returnFrom :: ProgramState -> IO ProgramState
returnFrom p@ProgramState{..} = return $ p  { stack = tail stack
                                            , programCounter = head stack
                                            }

-- | Jump to the given address.
-- ????
-- Is the address going to be off by 2 bytes, or do programs expect it?
jumpToAddr :: Word16 -> ProgramState -> IO ProgramState
jumpToAddr addr pState = return $ pState { programCounter = addr }

-- | Call the subroutine at the given address.
-- ???? 
-- do I need to modify either of the programCounter addresses?
callSubroutine :: Word16 -> ProgramState -> IO ProgramState
callSubroutine addr p@ProgramState{..} = return $ 
    p   { stack = (programCounter : stack)
        , programCounter = addr
        }

-- | Compare the register to the given immediate value using the given 
-- function.  Skip the next instruction if the function evaluates to True.
skipIfVal :: (Word8 -> Word8 -> Bool) -> Int -> Word8 -> ProgramState 
                -> IO ProgramState
skipIfVal op i val p@ProgramState{..} 
    | (registers ! i) `op` val = return $ 
                                    p { programCounter = programCounter + 2 }
    | otherwise = return $ p

-- | Compare the register values using the given function.  Skip the next 
-- instruction if the function evaluates to True.
skipIfReg :: (Word8 -> Word8 -> Bool) -> Int -> Int -> ProgramState
                -> IO ProgramState
skipIfReg op i1 i2 p@ProgramState{..}
    | (registers ! i1) `op` (registers ! i2) = return $
                                    p { programCounter = programCounter + 2 }
    | otherwise = return $ p

-- | Set the register to the given value.
-- ????
-- Want to test a non-thaw version of this function
setRegVal :: Int -> Word8 -> ProgramState -> IO ProgramState
setRegVal i val pState = do
    regs <- thaw $ registers pState
    write regs i val
    newRegs <- freeze regs
    return $ pState { registers = newRegs }

-- | Add the given value to the current value of the register.
-- Note:  Does not set the carry flag.
-- ????
-- Want to test a non-thaw version of this function
addRegVal :: Int -> Word8 -> ProgramState -> IO ProgramState
addRegVal i val pState = do
    regs <- thaw $ registers pState
    curVal <- MV.read regs i
    write regs i $ curVal + val
    newRegs <- freeze regs
    return $ pState { registers = newRegs }

-- | Set the first register to the value of the second.
setReg :: Int -> Int -> ProgramState -> IO ProgramState
setReg i1 i2 pState = do
    regs <- thaw $ registers pState
    (MV.read regs i2) >>= (write regs i1)
    newRegs <- freeze regs
    return $ pState { registers = newRegs }

-- | Set the first register to the value of the function with the value of 
-- both registers as arguments.
setRegOp :: (Word8 -> Word8 -> Word8) -> Int -> Int -> ProgramState
                -> IO ProgramState
setRegOp op i1 i2 pState = do
    regs <- thaw $ registers pState
    r1Val <- MV.read regs i1
    r2Val <- MV.read regs i2
    write regs i1 $ r1Val `op` r2Val
    newRegs <- freeze regs
    return $ pState { registers = newRegs }

-- | Set the first register to the value of the function with the value of 
-- both of the registers as arguments.  Also, set the carry flag based on the 
-- secondary result of the function.
setRegOpCarry :: (Word8 -> Word8 -> (Word8, Word8)) -> Int -> Int
                    -> ProgramState -> IO ProgramState
setRegOpCarry op i1 i2 pState = do
    regs <- thaw $ registers pState
    r1Val <- MV.read regs i1
    r2Val <- MV.read regs i2
    let (result, carry) = r1Val `op` r2Val
    write regs i1 result
    write regs 0xF carry
    newRegs <- freeze regs
    return $ pState { registers = newRegs }

-- | The same as setRegOpCarry except the order of the parameters is reversed.
setRegOpCarry2 :: (Word8 -> Word8 -> (Word8, Word8)) -> Int -> Int
                    -> ProgramState -> IO ProgramState
setRegOpCarry2 op i1 i2 pState = do
    regs <- thaw $ registers pState
    r1Val <- MV.read regs i1
    r2Val <- MV.read regs i2
    let (result, carry) = r2Val `op` r1Val
    write regs i1 result
    write regs 0xF carry
    newRegs <- freeze regs
    return $ pState { registers = newRegs }
    
-- | Shift the given register using the given function, and put that bit into 
-- the carry register.
shiftOutReg :: (Word8 -> Int -> Word8) -> Int -> Word8 -> ProgramState
                    -> IO ProgramState
shiftOutReg op i andVal pState = do
    regs <- thaw $ registers pState
    val <- MV.read regs i
    write regs 0xF $ val .&. andVal
    write regs i $ val `op` 1
    newRegs <- freeze regs
    return $ pState { registers = newRegs }


-- | Set the index register to the given address.
setIndex :: Word16 -> ProgramState -> IO ProgramState
setIndex addr pState = return $ pState { indexRegister = addr }

-- | Jump to the address plus register 0.
jumpToAddrReg :: Word16 -> ProgramState -> IO ProgramState
jumpToAddrReg addr pState = 
    let val = fromIntegral $ (registers pState) ! 0 in 
    jumpToAddr (addr + val) pState

--"Reg[nib2] = rand in [0,255] & low"
-- | Set the given register to a random number [0, 255] ANDed with the given 
-- value.
randGen :: Int -> Word8 -> ProgramState -> IO ProgramState
randGen i val pState = do
    regs <- thaw $ registers pState
    randNum <- getStdRandom $ randomR (0x00, 0xFF)
    write regs i $ randNum .&. val
    newRegs <- freeze regs
    return $ pState { registers = newRegs }

--"Draw sprite at pos (Reg[nib2], Reg[nib3])\
-- \ with width=8px height=nib4;\
-- \ each row[0-nib4] read 8 bits at a time from Mem[I];\
-- \ Reg[0xF] = 1 if any screen pixel goes from set to unset"
drawSprite :: Int -> Int -> Word8 -> ProgramState -> IO ProgramState
drawSprite i1 i2 val = noOp

-- | Skip the next instruction if the key stored in the given register is 
-- pressed.
skipIfKey :: Int -> Bool -> ProgramState -> IO ProgramState
skipIfKey i target p@ProgramState{..}
    | keyState ! (fromIntegral (registers ! i)) == target = return $
                                    p { programCounter = programCounter + 2 }
    | otherwise = return p

-- | Set the given register to the value in the delay timer.
getDelay :: Int -> ProgramState -> IO ProgramState
getDelay i pState = do
    regs <- thaw $ registers pState
    write regs i $ delayTimer pState
    newRegs <- freeze regs
    return $ pState { registers = newRegs }

-- | Block the program execution until a key is pressed, then set the given 
-- register to the value of that key.
getNextKey :: Int -> ProgramState -> IO ProgramState
getNextKey i p@ProgramState{..} =
    case elemIndex True keyState of
        (Just keyI) -> do
            regs <- thaw registers
            write regs i $ fromIntegral keyI
            newRegs <- freeze regs
            return $ p { registers = newRegs }
        (Nothing) -> return $ p { programCounter = programCounter - 2 }

-- | Set the delay timer to the value in the given register.
setDelay :: Int -> ProgramState -> IO ProgramState
setDelay i pState = let val = (registers pState) ! i in 
    return pState { delayTimer = val }

-- | Set the sound timer to the value in the given register.
setSound :: Int -> ProgramState -> IO ProgramState
setSound i pState = let val = (registers pState) ! i in 
    return pState { soundTimer = val }

-- | Add the value in the given register to the index register.
addToIndex :: Int -> ProgramState -> IO ProgramState
addToIndex i p@ProgramState{..} = let val = fromIntegral $ registers ! i in
    return p { indexRegister = indexRegister + val }

