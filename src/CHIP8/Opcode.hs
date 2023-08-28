{-|
Module      : Opcode
Description : Opcode decoding and execution
Copyright   : (c) Chad Reynolds, 2018-2023
License     : MIT
-}

{-# LANGUAGE RecordWildCards #-}

module Opcode (
    OpcodeComponents(..)
    , createOpcodeComponents
    , decodeOpcode
    , decodeOpcode'
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
    -- * Unused
    -- | call machine code routine at address
    CallRoutine Address
    -- * Display
    -- | clear the screen
    | ClearDisplay
    -- * Program flow
    -- | return from a subroutine
    | Return
    -- | jump to address
    | Jump Address
    -- | call subroutine at address
    | Subroutine Address
    -- | skip next instruction if value in register == immediate
    | SkipEq Register Immediate
    -- | skip next instruction if value in register != immediate
    | SkipNotEq Register Immediate
    -- | skip next instruction iv value in registers are equal
    | SkipRegEq Register Register
    -- | skip next instruction if values in registers are not equal
    | SkipRegNotEq Register Register
    -- | X = NN
    | Assign Register Immediate
    -- | X += NN (carry flag not changed)
    | AddValue Register Immediate
    -- | X = Y
    | AssignReg Register Register
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
    -- | X += Y
    | Add Register Register
    -- | X -= Y
    | Subtract Register Register
    -- | X = Y - X
    | SubtractFlip Register Register
    -- | set address register to address
    | SetIndex Address
    -- | jump to address plus value in register 0
    | JumpAdd Address
    -- | X = random() & NN
    | SetRandom Register Immediate
    -- | draw 8 by N sprite at (X,Y)
    | Draw Register Register NImmediate
    -- | skip next instruction if key stored in X is pressed
    | SkipKey Register
    -- | skip next instruction if key stored in X is not pressed
    | SkipNotKey Register
    -- | X = get_delay_timer()
    | GetTimer Register
    -- | X = get_key()
    | GetKey Register
    -- | set_delay_timer(X)
    | SetTimer Register
    -- | set_sound_timer(X)
    | SetSound Register
    -- | set address register to X
    | AddIndexReg Register
    -- | set address register to the location of the sprite for character in X
    | SetIndexSprite Register
    -- | set_binary_coded_decimal(X)
    | BCD Register
    -- | register dump 0 to X to memory
    | RegDump Register
    -- | register load 0 to X from memory
    | RegLoad Register
  deriving (Show)

decodeOpcode' :: OpcodeComponents -> Opcode
decodeOpcode' code@(OpcodeComponents{..}) =
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

-- | Decode the opcode according to the CHIP-8 specification and take the 
-- appropriate action on the ProgramState.
decodeOpcode :: OpcodeComponents -> ProgramState -> IO ProgramState
decodeOpcode code@(OpcodeComponents{..}) =
    case nibble1 of 
        0x0000 -> case address of 
                    0x00E0  -> clearScreen
                    0x00EE  -> returnFrom
                    _       -> noOp -- call program at addr
        0x0001 -> jumpToAddr address
        0x0002 -> callSubroutine address
        0x0003 -> skipIfVal (==) nibble2 low
        0x0004 -> skipIfVal (/=) nibble2 low
        0x0005 -> skipIfReg (==) nibble2 nibble3
        0x0006 -> setRegVal nibble2 low
        0x0007 -> addRegVal nibble2 low
        0x0008 -> case nibble4 of
                    0x0000 -> setReg nibble2 nibble3
                    0x0001 -> setRegOp (.|.) nibble2 nibble3
                    0x0002 -> setRegOp (.&.) nibble2 nibble3
                    0x0003 -> setRegOp xor nibble2 nibble3
                    0x0004 -> setRegOpCarry addCarry nibble2 nibble3
                    0x0005 -> setRegOpCarry subtractCarry nibble2 nibble3
                    0x0006 -> shiftOutReg shiftR nibble2 0x01
                    0x0007 -> setRegOpCarry2 subtractCarry nibble2 nibble2
                    0x000E -> shiftOutReg shiftL nibble2 0x80
                    _       -> error $ "Unexpected opcode:  " ++ show code
        0x0009 -> skipIfReg (/=) nibble2 nibble3
        0x000A -> setIndex address
        0x000B -> jumpToAddrReg address
        0x000C -> randGen nibble2 low
        0x000D -> drawSprite nibble2 nibble3 nibble4
        0x000E -> case low of
                    0x009E -> skipIfKey nibble2 True
                    0x00A1 -> skipIfKey nibble2 False
                    _       -> error $ "Unexpected opcode:  " ++ show code
        0x000F -> case low of
                    0x0007 -> getDelay nibble2
                    0x000A -> getNextKey nibble2
                    0x0015 -> setDelay nibble2
                    0x0018 -> setSound nibble2
                    0x001E -> addToIndex nibble2
                    0x0029 -> setIndexToFont nibble2
                    0x0033 -> binCD nibble2
                    0x0055 -> dumpFromRegs nibble2
                    0x0065 -> loadFromMem nibble2
                    _       -> error $ "Unexpected opcode:  " ++ show code
        _ -> error $ "Unexpected opcode:  " ++ show code

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
    p   { stack = programCounter : stack
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
    let pixelUpdate = if foldr (.|.) 0 updates > 0
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

