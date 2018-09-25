{-|
Module      : CHIP8.Main
Description : The main CHIP-8 program
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}

module CHIP8.Main (
    main
    ) where

import Data.Vector                  (empty, thaw)
import Data.Vector.Mutable  as MV   (read)

import CHIP8.Opcode                 (decodeOpcode)
import CHIP8.ProgramState           (ProgramState(..), initializeProgram)
import CHIP8.Util                   (decrementToZero, makeWord16)


-- | Initialize the program state, load the program into memory, then begin
-- execution loop.
main :: IO ()
main = do
    let pState = initializeProgram empty
    mainLoop pState

mainLoop :: ProgramState -> IO ()
mainLoop pState = do
    _ <- emulateCycle pState
    --update keyState
    --detect if quit key was pressed
    --when (screenUpdated nextState) (draw nextState)
    --unless ((done nextState) || isQuit) (mainLoop nextState)
    return ()

-- | Step through the execution cycle of a CHIP-8 program.
emulateCycle :: ProgramState -> IO ProgramState
emulateCycle pState = do
    mem <- thaw $ memory pState
    opcodeH <- MV.read mem $ fromIntegral (programCounter pState)
    opcodeL <- MV.read mem $ fromIntegral (1 + (programCounter pState))
    let opcodeAction = decodeOpcode (makeWord16 opcodeH opcodeL)
    nextState <- opcodeAction pState
    return $ nextState  { delayTimer = decrementToZero $ delayTimer nextState
                        , soundTimer = decrementToZero $ soundTimer nextState
                        , programCounter = 2 + programCounter nextState
                        }

