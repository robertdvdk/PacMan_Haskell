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
    createFile
    highScores      <- readF
    stage1          <- loadBMP "pac-man1.bmp"
    stage2          <- loadBMP "pac-man2.bmp"
    stage3          <- loadBMP "pac-man3.bmp"
    stage4          <- loadBMP "pac-man4.bmp"
    stage5          <- loadBMP "pac-man5.bmp"
    redGhostBM      <- loadBMP "redGhost.bmp"
    pinkGhostBM     <- loadBMP "pinkGhost.bmp"
    yellowGhostBM   <- loadBMP "yellowGhost.bmp"
    blueGhostBM     <- loadBMP "blueGhost.bmp"

    let initialPlayer = Player [stage1, stage2, stage3, stage4, stage5] (0, 0) West West 0 3

    let initialGhosts1 = [Ghost redGhostBM (0, 0) West InsideCage 0 1, Ghost pinkGhostBM (0, 0) West InsideCage 1 2]
    let initialLevel1 = Level blue  maze1 cage1 initialGhosts1 food1 largefood1 playerSpawn1 ghostsSpawn1 0

    let initialGhosts2 = [Ghost redGhostBM (0, 0) West InsideCage 0 1, Ghost pinkGhostBM (0, 0) West InsideCage 1 2, Ghost yellowGhostBM (0, 0) West InsideCage 2 3]
    let initialLevel2 = Level green maze2 cage2 initialGhosts2 food2 largefood2 playerSpawn2 ghostsSpawn2 0

    let initialGhosts3 = [Ghost redGhostBM (0, 0) West InsideCage 0 1, Ghost pinkGhostBM (0, 0) West InsideCage 1 2, Ghost yellowGhostBM (0, 0) West InsideCage 2 3, Ghost blueGhostBM (0, 0) West InsideCage 5 4]
    let initialLevel3 = Level red   maze3 cage3 initialGhosts3 food3 largefood3 playerSpawn3 ghostsSpawn3 0

    let initialState = GameState Start initialPlayer initialLevel1 [initialLevel1, initialLevel2, initialLevel3] 0 highScores 1

    playIO (InWindow "Pac-Man" (550, 550) (0, 0)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function