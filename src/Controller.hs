-- | This module defines how the state changes
-- | in response to time and user input
module Controller where

import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import ReadWrite
import Data.List
import Movement
import Levels
import Control.Monad.State

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = case playState gstate of
      Start     -> do                       
                      highScores  <- readF
                      return $ gstate { level = resetLevel (level gstate) (levels gstate), 
                        player = (player gstate) { dyingTimer = 0 }, highScores = highScores }
      Playing   -> updateGameState gstate
      GameOver  -> do   
                      writeF gstate
                      dyingAnimation gstate
      _         -> return $ gstate 

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

dyingAnimation :: GameState -> IO GameState
dyingAnimation gstate = 
  do 
    let gstate' = gstate { player = setTimer (player gstate) }
    return gstate' -- { redGhost = (redGhost gstate) { ghostLocation = (0, 15), ghostOutsideCage = InsideCage } }

-- | Increments the number of frames for the dying animation
setTimer :: Player -> Player
setTimer player = player { dyingTimer = (dyingTimer player + 1) }

-- | Update player's direction, location, and ghosts' location
updateGameState :: GameState -> IO GameState
updateGameState gstate = 
  do 
    let gstate'     = movePlayer (gstate { player = playerChangeDirection (maze (level gstate)) (player gstate) })
    let gstate''    = eatFood gstate'
    let gstate'''   = gstate''  { level = flashCage (level gstate'') }
    let gstate''''  = gstate''' { level = (level gstate) { ghosts = checkGhostInCage (ghostCage (level gstate''')) (ghosts (level gstate''')) } }
    movedGhosts <- moveGhost gstate'''' (ghosts (level gstate''''))
    let gstate''''' = gstate'''' { level = (level gstate) { ghosts = movedGhosts } }
    if checkPlayerGhostCollision gstate''''' (ghosts (level gstate))
      then (return gstate''''' { playState = GameOver }) 
      else if checkEverythingEaten gstate'''''
            then return gstate''''' { playState = Win }
            else return gstate'''''

-- | Increments the number of frames for the cage; the cages flips between yellow and blue for 5 frames each
flashCage :: Level -> Level
flashCage level | cageTimer level == 10  = level { cageTimer = 0 }
                | otherwise              = level { cageTimer = (cageTimer level + 1) }
                            
-- | PURE PART STARTS HERE
inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) Down _ _) gstate
    | c == 'w'    = changeDirection North
    | c == 's'    = changeDirection South
    | c == 'a'    = changeDirection West
    | c == 'd'    = changeDirection East
    | c == 'p'    = changeGameState gstate
    | c == 'g'    = gstate { playState = GameOver }                                   -- TEST GAMEOVER
    | c == 't'    = gstate { playState = Win }                                        -- TEST WIN
    | c == '1'    = gstate { level = level1 }                                         -- Go to level 1
    | c == '2'    = gstate { level = level2 }                                         -- Go to level 2
    | c == '3'    = gstate { level = level3 }                                         -- Go to level 3
    | c == 'h'    = gstate { highScores = [0, 0, 0, 0, 0] }                           -- Reset HIGH SCORES
    where   [level1, level2, level3] = levels gstate
            changeDirection direction = gstate { player = playerChangeNextDirection direction (player gstate)}
              where playerChangeNextDirection dir player = player { playerNextDirection = dir }
inputKey _ gstate = gstate

-- | Changes the state based on the current state and pressed key
changeGameState :: GameState -> GameState 
changeGameState gstate = case playState gstate of 
  Start     -> gstate { playState = Playing }
  Playing   -> gstate { playState = Paused }
  Paused    -> gstate { playState = Playing }
  GameOver  -> gstate { playState = Start, score = 0 }
  Win       -> gstate { playState = Start, level = nextLevel (level gstate) (levels gstate) }

resetLevel :: Level -> [Level] -> Level
resetLevel level [level1, level2, level3] = level -- Moet zo gefikst worden dat ie de goede initialLevel haalt uit de lijst met levels.

nextLevel :: Level -> [Level] -> Level            -- Zelfde maar dan het volgende level.
nextLevel level [level1, level2, level3] = level3