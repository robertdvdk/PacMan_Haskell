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

    let initialPlayer = Player [stage1, stage2, stage3, stage4, stage5] (0, 0) West West 0
    
    let initialLevel1 = Level blue  maze1 cage1 initialGhosts1 food1 largefood1 playerSpawn1 0
    let playerSpawn1 = (0, 0)
    let initialGhosts1 = [Ghost redGhostBM (0, 15) West East InsideCage 0]

    let initialLevel2 = Level green maze2 cage2 initialGhosts2 food2 largefood2 playerSpawn2 0
    let playerSpawn2 = (22, 22)
    let initialGhosts2 = [Ghost redGhostBM (-22, -20) West East InsideCage 0, Ghost blueGhostBM 3 (-22, -21) West West InsideCage 3]

    let initialLevel3 = Level red   maze3 cage3 initialGhosts3 food3 largefood3 playerSpawn3 0
    let playerSpawn3 = (0, 0)
    let initialGhosts3 = [Ghost 0 redGhostBM (22, 3) West West InsideCage 0, Ghost blueGhostBM (21, 2) West West InsideCage 1, 
        Ghost yellowGhostBM (22, -2) West West InsideCage 10]

    let initialState = GameState Start initialPlayer [initialLevel1, initialLevel2, initialLevel3] highScores

    playIO (InWindow "Pac-Man" (500, 500) (0, 0)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function