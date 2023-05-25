{-|
Module      : DisplayState
Description : Holds all of the data needed for the visuals
Copyright   : (c) Chad Reynolds, 2018-2023
License     : MIT
-}


module DisplayState (
      DisplayState(..)
    ) where

import SDL.Video            (Window)
import SDL.Video.Renderer   (Renderer)


-- | Holds the needed SDL data for drawing on the screen.
data DisplayState = DisplayState { 
      window        :: Window
    , renderer      :: Renderer
    }

