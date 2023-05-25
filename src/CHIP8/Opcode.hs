{-|
Module      : Opcode
Description : Opcode decoding and execution
Copyright   : (c) Chad Reynolds, 2018-2023
License     : MIT
-}

{-# LANGUAGE RecordWildCards #-}

module Opcode (
    decodeOpcode
    ) where

import Data.Bits                    ((.&.), (.|.), shiftL, shiftR, xor)
import Data.Word                    (Word8, Word16)
import Data.Vector          as V    ((!), elemIndex, freeze, fromList, slice, 
                                        thaw, zip)
import Data.Vector.Mutable  as MV   (IOVector, read, set, write)

import System.Random                (getStdRandom, randomR)

import ProgramState           (ProgramState(..), fontDataAddr)
import Util                   (addCarry, makeWord16, splitWord16, 
                                        subtractCarry)


-- | Decode the opcode according to the CHIP-8 specification and take the 
-- appropriate action on the ProgramState.
decodeOpcode :: Word16 -> ProgramState -> IO ProgramState
decodeOpcode code =
    case nib1 of 
        0x0000 -> case addr of 
                    0x00E0  -> clearScreen
                    0x00EE  -> returnFrom
                    _       -> noOp -- call program at addr
        0x0001 -> jumpToAddr addr
        0x0002 -> callSubroutine addr
        0x0003 -> skipIfVal (==) nib2 low
        0x0004 -> skipIfVal (/=) nib2 low
        0x0005 -> skipIfReg (==) nib2 nib3
        0x0006 -> setRegVal nib2 low
        0x0007 -> addRegVal nib2 low
        0x0008 -> case nib4 of
                    0x0000 -> setReg nib2 nib3
                    0x0001 -> setRegOp (.|.) nib2 nib3
                    0x0002 -> setRegOp (.&.) nib2 nib3
                    0x0003 -> setRegOp (xor) nib2 nib3
                    0x0004 -> setRegOpCarry addCarry nib2 nib3
                    0x0005 -> setRegOpCarry subtractCarry nib2 nib3
                    0x0006 -> shiftOutReg shiftR nib2 0x01
                    0x0007 -> setRegOpCarry2 subtractCarry nib2 nib2
                    0x000E -> shiftOutReg shiftL nib2 0x80
                    _       -> error $ "Unexpected opcode:  " ++ (show code)
        0x0009 -> skipIfReg (/=) nib2 nib3
        0x000A -> setIndex addr
        0x000B -> jumpToAddrReg addr
        0x000C -> randGen nib2 low
        0x000D -> drawSprite nib2 nib3 nib4
        0x000E -> case low of
                    0x009E -> skipIfKey nib2 True
                    0x00A1 -> skipIfKey nib2 False
                    _       -> error $ "Unexpected opcode:  " ++ (show code)
        0x000F -> case low of
                    0x0007 -> getDelay nib2
                    0x000A -> getNextKey nib2
                    0x0015 -> setDelay nib2
                    0x0018 -> setSound nib2
                    0x001E -> addToIndex nib2
                    0x0029 -> setIndexToFont nib2
                    0x0033 -> binCD nib2
                    0x0055 -> dumpFromRegs nib2
                    0x0065 -> loadFromMem nib2
                    _       -> error $ "Unexpected opcode:  " ++ (show code)
        _ -> error $ "Unexpected opcode:  " ++ (show code)
    where 
        nib1 = fromIntegral ((code .&. 0xF000) `shiftR` 24) :: Int
        nib2 = fromIntegral ((code .&. 0x0F00) `shiftR` 16) :: Int
        nib3 = fromIntegral ((code .&. 0x00F0) `shiftR` 8) :: Int
        nib4 = fromIntegral (code .&. 0x000F) :: Int
        addr = code .&. 0x0FFF
        low  = fromIntegral (code .&. 0x00FF) :: Word8

-- | Do nothing and return the state unchanged.  Used as a placeholder until 
-- opcodes are implemented.
noOp :: ProgramState -> IO ProgramState
noOp = return

-- | Clear the screen, setting all the bits to 0.
clearScreen :: ProgramState -> IO ProgramState
clearScreen pState = do
    scr <- thaw $ screen pState
    set scr 0
    newScreen <- freeze scr
    return $ pState { screen = newScreen
                    , screenModified = True
                    }

-- | Return from a subroutine.
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
-- Is the address going to be off by 2 bytes, or do programs expect it?
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
setRegVal :: Int -> Word8 -> ProgramState -> IO ProgramState
setRegVal i val pState = do
    regs <- thaw $ registers pState
    write regs i val
    newRegs <- freeze regs
    return $ pState { registers = newRegs }

-- | Add the given value to the current value of the register.
-- Note:  Does not set the carry flag.
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

-- | Draw a sprite at the screen location indexed by the two given register 
-- values, it is 8-bits wide and byteNum rows tall.  The sprite is located in 
-- memory at the address in index register, and consists of the given byteNum 
-- number of bytes.
-- Note;  The carry flag is set to 1 if any screen pixel goes from set to 
-- unset, 0 otherwise.  This is used for collision detection.
drawSprite :: Int -> Int -> Int -> ProgramState -> IO ProgramState
drawSprite i1 i2 byteNum p@ProgramState{..} = do
    scr <- thaw screen
    updates <- sequence $ map (\rowCount -> helper rowCount scr) 
                                [0..byteNum]
    let pixelUpdate = if (foldr (.|.) 0 updates) > 0
                        then 1
                        else 0
    regs <- thaw registers
    write regs 0xF pixelUpdate
    newRegs <- freeze regs
    newScreen <- freeze scr
    return $ p  { screen = newScreen
                , registers = newRegs
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
            let curWord = makeWord16 b1 b2
                newData = fromIntegral (spriteData ! i) `shiftL` b1LShift
                newWord =  curWord `xor` newData
                updated = curWord .&. newData
                (b1', b2') = splitWord16 newWord
            write mScreen screenIndex b1'
            write mScreen screenIndex b2'
            return updated

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

-- | Set the index register to the memory address of the image data for the 
-- digit stored in the given register.
setIndexToFont :: Int -> ProgramState -> IO ProgramState
setIndexToFont i pState = let   char = fromIntegral $ (registers pState) ! i 
                                addr = char * 5 + fontDataAddr in
    return $ pState { indexRegister =  addr }

-- | Store the binary-coded decimal representation of the number in the 
-- given register in memory where the index register is pointing to.
binCD :: Int -> ProgramState -> IO ProgramState
binCD i pState = let    index = fromIntegral $ indexRegister pState
                        val = (registers pState) ! i 
                        ones = val `rem` 10
                        tens = val `div` 10 `rem` 10
                        huns = val `div` 100 in do
    mem <- thaw $ memory pState
    write mem index huns
    write mem (index + 1) tens
    write mem (index + 2) ones
    newMem <- freeze mem
    return $ pState { memory = newMem }

-- | Dump registers 0 to the given num into memory starting at the address 
-- in the index register.
dumpFromRegs :: Int -> ProgramState -> IO ProgramState
dumpFromRegs n pState = let index = fromIntegral $ indexRegister pState in
    do
        mem <- thaw $ memory pState
        sequence_ $ fmap (helper mem index) 
                        $ V.zip (fromList [0..n]) (registers pState)
        newMem <- freeze mem
        return $ pState { memory = newMem }
    where
        helper :: IOVector Word8 -> Int -> (Int, Word8) -> IO ()
        helper m index (i, val) = write m (index + i) val

-- | Load data from memory into the registers 0 to the given num starting at 
-- the address in the index register.
loadFromMem :: Int -> ProgramState -> IO ProgramState
loadFromMem n p@ProgramState{..} = 
    let index = fromIntegral indexRegister
        memData = V.zip (fromList [0..n]) (slice index n memory) in
    do
        regs <- thaw registers
        sequence_ $ fmap (helper regs) memData
        newRegs <- freeze regs
        return $ p { registers = newRegs }
    where
        helper :: IOVector Word8 -> (Int, Word8) -> IO ()
        helper r (i, val) = write r i val

