module Main where

import Controller
import Model
import View
import ReadWrite
import Movement
import Levels

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Bitmap

main :: IO ()
main = do
    highScores  <- readF
    stage1      <- loadBMP "pac-man1.bmp"
    stage2      <- loadBMP "pac-man2.bmp"
    stage3      <- loadBMP "pac-man2.bmp"
    stage4      <- loadBMP "pac-man2.bmp"
    stage5      <- loadBMP "pac-man2.bmp"
    redGhost    <- loadBMP "pac-man2.bmp"
    let pacManStages = [stage1, stage2, stage3, stage4, stage5]
    let ghostBitMaps = [redGhost, redGhost, redGhost, redGhost]
    playIO (InWindow "Pac-Man" (500, 500) (0, 0)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              initialState { pacManStages = pacManStages, ghostBitMaps = ghostBitMaps, highScores = highScores }     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function