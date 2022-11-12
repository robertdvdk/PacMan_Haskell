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
    highScores      <- readF
    stage1          <- loadBMP "C:/Users/Jurre Luijten/Documents/Master/Functioneel Programmeren/Game/Pac-Man/pac-man1.bmp"
    stage2          <- loadBMP "C:/Users/Jurre Luijten/Documents/Master/Functioneel Programmeren/Game/Pac-Man/pac-man2.bmp"
    stage3          <- loadBMP "C:/Users/Jurre Luijten/Documents/Master/Functioneel Programmeren/Game/Pac-Man/pac-man3.bmp"
    stage4          <- loadBMP "C:/Users/Jurre Luijten/Documents/Master/Functioneel Programmeren/Game/Pac-Man/pac-man4.bmp"
    stage5          <- loadBMP "C:/Users/Jurre Luijten/Documents/Master/Functioneel Programmeren/Game/Pac-Man/pac-man5.bmp"
    redGhostBM      <- loadBMP "C:/Users/Jurre Luijten/Documents/Master/Functioneel Programmeren/Game/Pac-Man/redGhost.bmp"
    pinkGhostBM     <- loadBMP "C:/Users/Jurre Luijten/Documents/Master/Functioneel Programmeren/Game/Pac-Man/redGhost.bmp"
    yellowGhostBM   <- loadBMP "C:/Users/Jurre Luijten/Documents/Master/Functioneel Programmeren/Game/Pac-Man/redGhost.bmp"
    blueGhostBM     <- loadBMP "C:/Users/Jurre Luijten/Documents/Master/Functioneel Programmeren/Game/Pac-Man/redGhost.bmp"

    let initialRedGhost = Ghost redGhostBM (0, 15) West East Red InsideCage
    let initialPlayer = Player [stage1, stage2, stage3, stage4, stage5] (0, 0) West West 0
    let initialState = GameState Start initialPlayer level1 10 highScores [initialRedGhost, None, None, None]

    playIO (InWindow "Pac-Man" (500, 500) (0, 0)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function