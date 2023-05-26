{-|
Module      : Main
Description : The main CHIP-8 program
Copyright   : (c) Chad Reynolds, 2018-2023
License     : MIT
-}

{-# LANGUAGE RecordWildCards #-}

module Main (
    main
    ) where

import Control.Monad                (unless)
import Data.Vector                  (Vector, convert, empty, thaw)
import Data.Vector.Mutable  as MV   (read)
import qualified Data.Vector.Storable as SV (Vector, thaw)
import Data.Word                    (Word8)

import SDL.Init                     (initializeAll, quit)
import SDL.Event                    (Event, pollEvents)
import SDL.Vect                     (Point(P), V2(V2), V4(V4))
import SDL.Video                    (destroyWindow)
import SDL.Video.Renderer           (createRGBSurfaceFrom, getWindowSurface, masksToPixelFormat, present, surfaceBlit, updateWindowSurface)

import DisplayState           (DisplayState(DisplayState, renderer, window), createDisplayState)
import Opcode                 (decodeOpcode)
import ProgramState           (ProgramState(..), KeyState, initializeProgram)
import Util                   (decrementToZero, makeWord16)


-- | Initialize the program state, load the program into memory, then begin
-- execution loop.
main :: IO ()
main = do
    initializeAll
    let pState = initializeProgram empty
    ds <- createDisplayState
    mainLoop pState ds
    destroyWindow $ window ds
    quit

mainLoop :: ProgramState -> DisplayState -> IO ()
mainLoop pState dState = do
    events <- pollEvents
    let ks' = foldr updateKeyState (keyState pState) events
        isQuit = True   -- actually check for a quit key like Esc
    nextState <- emulateCycle pState { keyState = ks' }
    nextState' <- if screenModified pState 
                    then renderScreen nextState dState
                    else return nextState
    -- delay until a 60th of a second has passed
    unless isQuit (mainLoop nextState' dState)  -- also check if emu is done

-- | Step through the execution cycle of a CHIP-8 program.
emulateCycle :: ProgramState -> IO ProgramState
emulateCycle pState = do
    mem <- thaw $ memory pState
    opcodeH <- MV.read mem $ fromIntegral (programCounter pState)
    opcodeL <- MV.read mem $ fromIntegral (1 + programCounter pState)
    let opcodeAction = decodeOpcode (makeWord16 opcodeH opcodeL)
    nextState <- opcodeAction pState
    return $ nextState  { delayTimer = decrementToZero $ delayTimer nextState
                        , soundTimer = decrementToZero $ soundTimer nextState
                        , programCounter = 2 + programCounter nextState
                        }

renderScreen :: ProgramState -> DisplayState -> IO ProgramState
renderScreen pState DisplayState{..} = do
    scrM <- SV.thaw $ (convert :: Vector Word8 -> SV.Vector Word8) (screen pState)
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

updateKeyState :: Event -> KeyState -> KeyState
updateKeyState event ks = undefined

{- 
keycodes
    Keypad                   Keyboard
    +-+-+-+-+                +-+-+-+-+
    |1|2|3|C|                |1|2|3|4|
    +-+-+-+-+                +-+-+-+-+
    |4|5|6|D|                |Q|W|E|R|
    +-+-+-+-+       =>       +-+-+-+-+
    |7|8|9|E|                |A|S|D|F|
    +-+-+-+-+                +-+-+-+-+
    |A|0|B|F|                |Z|X|C|V|
    +-+-+-+-+                +-+-+-+-+
-}

