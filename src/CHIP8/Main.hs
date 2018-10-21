{-|
Module      : CHIP8.Main
Description : The main CHIP-8 program
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}

{-# LANGUAGE RecordWildCards #-}

module CHIP8.Main (
    main
    ) where

import Data.Vector                  (Vector, convert, empty, thaw)
import Data.Vector.Mutable  as MV   (read)
import qualified Data.Vector.Storable as SV (Vector, thaw)
import Data.Word                    (Word8)

import SDL.Vect                     (Point(P), V2(V2), V4(V4))
import SDL.Video.Renderer           (createRGBSurfaceFrom, getWindowSurface, 
                                        masksToPixelFormat, present, 
                                        surfaceBlit, updateWindowSurface)

import CHIP8.DisplayState           (DisplayState(..), cleanUpDisplayState, 
                                        defaultDisplayState, updateDisplay)
import CHIP8.Opcode                 (decodeOpcode)
import CHIP8.ProgramState           (ProgramState(..), initializeProgram)
import CHIP8.Util                   (decrementToZero, makeWord16)


-- | Initialize the program state, load the program into memory, then begin
-- execution loop.
main :: IO ()
main = do
    let pState = initializeProgram empty
    dState <- defaultDisplayState
    mainLoop pState dState
    cleanUpDisplayState dState

mainLoop :: ProgramState -> DisplayState -> IO ()
mainLoop pState dState = do
    nextState <- emulateCycle pState
    -- update keyState
    -- detect if quit key was pressed
    nextState' <- if screenModified pState 
                    then renderScreen nextState dState
                    else return nextState
    -- delay until a 60th of a second has passed
    --unless (isQuit || done nextState) (mainLoop nextState)
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

renderScreen :: ProgramState -> DisplayState -> IO ProgramState
renderScreen pState DisplayState{..} = do
    scrM <- SV.thaw $ ((convert :: Vector Word8 -> SV.Vector Word8) (screen pState))
    -- expand each bit to RGBA
    pixFormat <- masksToPixelFormat bitsPerPixel rgbaMasks
    screenSurface <- createRGBSurfaceFrom scrM size pitch pixFormat
    winSurface <- getWindowSurface window
    _ <- surfaceBlit screenSurface Nothing winSurface destPos
    updateWindowSurface window
    present renderer
    return pState { screenModified = False }
    where
        size = V2 64 32
        bitsPerPixel = 8 * 4
        pitch = 64 * 4
        rgbaMasks = V4 0 0 0 0
        destPos = Just $ P (V2 0 0)

