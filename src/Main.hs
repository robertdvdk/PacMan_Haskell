module Main where

import Controller
import Model
import View
import ReadWrite
import Movement
import Levels

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
    highScores <- readF
    playIO (InWindow "Pac-Man" (500, 500) (0, 0)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              initialState { highScores = highScores }     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function