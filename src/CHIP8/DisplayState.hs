{-|
Module      : CHIP8.DisplayState
Description : Holds all of the data needed for the visuals
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}

{-# LANGUAGE OverloadedStrings #-}

module CHIP8.DisplayState (
      DisplayState(..)
    , cleanUpDisplayState
    , defaultDisplayState
    , updateDisplay
    ) where

import SDL.Video            (Window, createRenderer, createWindow, 
                                defaultWindow, destroyWindow)
import SDL.Video.Renderer   (Renderer, defaultRenderer, present)


-- | Holds the needed SDL data for drawing on the screen.
data DisplayState = DisplayState { 
      window        :: Window
    , renderer      :: Renderer
    }

-- | Matching demo settings to the default RaycasterState.
defaultDisplayState :: IO DisplayState
defaultDisplayState = do
    w <- createWindow "HaskCHIP8" defaultWindow
    r <- createRenderer w (-1) defaultRenderer
    return $ DisplayState w r 

--defaultWindowConfig :: WindowConfig
--defaultWindowConfig = defaultWindow { windowInitialSize = windowSize }

-- | Prompt the window to display the rendering actions taken since the last 
--  update.
updateDisplay :: DisplayState -> IO ()
updateDisplay dState = present (renderer dState)

-- | Call the proper functions for shutting down the state for SDL datatypes.
cleanUpDisplayState :: DisplayState -> IO ()
cleanUpDisplayState dState = do
    destroyWindow $ window dState

