-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES = 
    -- do return $ updateGameState (gstate {infoToShow = ShowAPlayer (player gstate)}) --  Make a bigger update function that updates the player (if pac man can change direction, change direction)
    do return $ updateGameState (gstate {infoToShow = ShowGame (level gstate) (player gstate)}) --  Make a bigger update function that updates the player (if pac man can change direction, change direction)
                              -- and doe movePlayer or later or make it part of updatePlayer    
    -- movePlayer (GameState (ShowAPlayer (player gstate)) 0 Playing (player gstate))
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
ableToChangeDirection level player = not (wallInDirection level (playerNextDirection player) player)

playerChangeDirection :: Level -> Player -> Player -- Needs to be checked the whole time, how?
playerChangeDirection level player  | ableToChangeDirection level player = player { playerDirection = playerNextDirection player}  
                                    | otherwise = player       -- If pac-man can make a move fill nextdirection in direction

movePlayer :: GameState -> GameState  
movePlayer gstate | not (wallInDirection (level gstate) (playerDirection (player gstate)) (player gstate)) = gstate {player = move (player gstate)}
                  | otherwise = gstate

wallInDirection :: Level -> Direction -> Player -> Bool -- Check if there is a wall in the given direction: generates a list of 10 points from the player towards the given direction
                                                        -- and checks if any of those points are in the 'level' list that contains the walls
wallInDirection level dir player = case dir of
  West -> (x - 1, y) `elem` level
  East -> (x + 1, y) `elem` level
  North -> (x, y + 1) `elem` level
  South -> (x, y - 1) `elem` level
  where (x, y) = playerLocation player
  -- West -> any (==True) [(x, y) `elem` level | x <- [u -15 .. u], y <- [v - 10, v, v + 10]]
  -- East -> any (==True) [(x, y) `elem` level | x <- [u .. u + 15], y <- [v - 10, v, v + 10]]
  -- North -> any (==True) [(x, y) `elem` level | x <- [u - 10, u, u + 10], y <- [v .. v + 15]]
  -- South -> any (==True) [(x, y) `elem` level | x <- [u - 10, u, u + 10], y <- [v -15 .. v]]
  -- where (u, v) = playerLocation player

updateGameState :: GameState -> GameState -- Update player's direction, location, and ghosts' location
updateGameState gstate = movePlayer (gstate {player = playerChangeDirection (level gstate) (player gstate)})

move :: Player -> Player          -- Change location based on the direction 
move player = case playerDirection player of 
  West -> player { playerLocation = (x - 1, y) }
  East -> player { playerLocation = (x + 1, y) }
  North -> player { playerLocation = (x, y + 1) }
  South -> player { playerLocation = (x, y - 1) }
  where (x, y) = playerLocation player

pauseGame :: GameState -> IO GameState
pauseGame gstate = do c <- getChar
                      case c of
                            ' ' -> case playState gstate of
                              Playing -> return $ gstate {playState = Paused}
                              Paused  -> return $ gstate {playState = Playing}
