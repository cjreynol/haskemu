{-|
Module      : DisplayState
Description : Holds all of the data needed for the visuals
Copyright   : (c) Chad Reynolds, 2018-2023
License     : MIT
-}

{-# LANGUAGE OverloadedStrings #-}

module DisplayState (
      DisplayState(..)
    , createDisplayState
    ) where

import SDL.Video            (Window, createRenderer, createWindow, defaultWindow)
import SDL.Video.Renderer   (Renderer, defaultRenderer)


-- | Holds the needed SDL data for drawing on the screen.
data DisplayState = DisplayState { 
      window        :: Window
    , renderer      :: Renderer
    }

createDisplayState :: IO DisplayState
createDisplayState = do
  w <- createWindow "CHIP-8 Emulator" defaultWindow
  r <- createRenderer w (-1) defaultRenderer
  return DisplayState {window = w, renderer = r}

-- TODO CJR: create standard DisplayState here, so Main can simply utilize it
