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

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = case playState gstate of
      Begin     -> do 
                      highScores <- readF
                      return $ initialState { highScores = highScores }
      Start     -> do
                      return $ gstate { player = initialPlayer, level = level2, ghost1 = firstGhost }
      Playing   -> updateGameState gstate
      GameOver  -> do   
                      writeF gstate
                      return $ gstate
      Win       -> do
                      return $ gstate
      _         -> return $ gstate 

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

-- | Update player's direction, location, and ghosts' location
updateGameState :: GameState -> IO GameState
updateGameState gstate = 
  do 
    let gstate' = movePlayer (gstate {player = playerChangeDirection (maze (level gstate)) (player gstate)})
    let gstate'' = eatFood gstate'
    let gstate''' = flashCage gstate''
    let gstate'''' = gstate''' {ghost1 = checkGhostInCage (ghostCage (level gstate''')) (ghost1 gstate''')}
    gstate''''' <- moveGhost gstate''''
    if checkPlayerGhostCollision gstate''''' 
      then (return gstate''''' { playState = GameOver }) 
      else return gstate'''''
    if True -- Needs function to check if all food is eaten
      then return gstate''''' { playState = Win }
      else return gstate'''''
                            
-- | PURE PART STARTS HERE
inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) Down _ _) gstate
    | c == 'w'    = changeDirection North
    | c == 's'    = changeDirection South
    | c == 'a'    = changeDirection West
    | c == 'd'    = changeDirection East
    | c == 'p'    = gstate { playState = changePlayState gstate }
    | c == 'g'    = gstate { playState = GameOver }                     -- TEST GAMEOVER
    | c == 'h'    = gstate { highScores = [20, 20, 0, 20, 20] }         -- TEST HIGH SCORES
    where changeDirection direction = gstate { player = playerChangeNextDirection direction (player gstate)}
            where playerChangeNextDirection dir player = player { playerNextDirection = dir }
inputKey _ gstate = gstate 

-- | Changes the state based on the current state and pressed key
changePlayState :: GameState -> PlayState 
changePlayState gstate = case playState gstate of 
  Begin     -> Playing
  Start     -> Playing
  Playing   -> Paused
  Paused    -> Playing
  GameOver  -> Begin
  Win       -> Start