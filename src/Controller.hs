-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Model (Player)

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
    do return $ updatePlayer --  Make a bigger update function that updates the player (if pac man can change direction, change direction)
                              -- and doe movePlayer or later or make it part of updatePlayer    
    movePlayer (GameState (ShowAPlayer (player gstate)) 0 Playing (player gstate))
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate
    | c == 'w'    = gstate { player = playerChangeNextDirection North (player gstate)}
    | c == 's'    = gstate { player = playerChangeNextDirection South (player gstate)}
    | c == 'a'    = gstate { player = playerChangeNextDirection West (player gstate)}
    | c == 'd'    = gstate { player = playerChangeNextDirection East (player gstate)}
    | otherwise   = gstate { infoToShow = ShowAChar c }
inputKey _ gstate = gstate 

playerChangeNextDirection :: Direction -> Player -> Player  -- When wasd is pressed, first the playerNextDirection changes
playerChangeNextDirection dir player = player { playerNextDirection = dir }

ableToChangeDirection :: Level -> Player -> Bool  -- Checks if pac-man can make a turn (now set to true because there is no check yet)
ableToChangeDirection level player = True

playerChangeDirection :: Level -> Player -> Player -- Needs to be checked the whole time, how?
playerChangeDirection level player  | ableToChangeDirection level player = player { playerDirection = playerNextDirection player}  
                                    | Otherwise = Nothing       -- If pac-man can make a move fill nextdirection in direction

movePlayer :: GameState -> GameState  
movePlayer gstate = gstate { elapsedTime = 0, player = move (player gstate) }

move :: Player -> Player          -- Change location based on the direction 
move player = case playerDirection player of 
  West -> player { playerLocation = (x - 5, y) }
  East -> player { playerLocation = (x + 5, y) }
  North -> player { playerLocation = (x, y + 5) }
  South -> player { playerLocation = (x, y - 5) }
  where (x, y) = playerLocation player

pauseGame :: GameState -> IO GameState
pauseGame gstate = do c <- getChar
                      case c of
                            ' ' -> case playState gstate of
                              Playing -> return $ gstate {playState = Paused}
                              Paused  -> return $ gstate {playState = Playing}

