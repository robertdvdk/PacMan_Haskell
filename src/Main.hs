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
    stage1      <- loadBMP "C:/Users/Jurre Luijten/Documents/Master/Functioneel Programmeren/Game/Pac-Man/pac-man1.bmp"
    stage2      <- loadBMP "C:/Users/Jurre Luijten/Documents/Master/Functioneel Programmeren/Game/Pac-Man/pac-man2.bmp"
    stage3      <- loadBMP "C:/Users/Jurre Luijten/Documents/Master/Functioneel Programmeren/Game/Pac-Man/pac-man3.bmp"
    stage4      <- loadBMP "C:/Users/Jurre Luijten/Documents/Master/Functioneel Programmeren/Game/Pac-Man/pac-man4.bmp"
    stage5      <- loadBMP "C:/Users/Jurre Luijten/Documents/Master/Functioneel Programmeren/Game/Pac-Man/pac-man5.bmp"
    redGhost    <- loadBMP "C:/Users/Jurre Luijten/Documents/Master/Functioneel Programmeren/Game/Pac-Man/RedGhost.bmp"
    let pacManStages = [stage1, stage2, stage3, stage4, stage5]
    let ghostBitMaps = [redGhost, redGhost, redGhost, redGhost]
    playIO (InWindow "Pac-Man" (500, 500) (0, 0)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              initialState { pacManStages = pacManStages, ghostBitMaps = ghostBitMaps, highScores = highScores }     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function