-- | This module defines how the state changes
--   in response to time and user input
module Controller where
import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = case playState gstate of
      Playing -> return $ updateGameState gstate
      _       -> return $ gstate 

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

-- Pure Part starts here
inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate
    | c == 'w'    = gstate { player = playerChangeNextDirection North (player gstate)}
    | c == 's'    = gstate { player = playerChangeNextDirection South (player gstate)}
    | c == 'a'    = gstate { player = playerChangeNextDirection West (player gstate)}
    | c == 'd'    = gstate { player = playerChangeNextDirection East (player gstate)}
    | c == 'p'    = gstate { playState = changePlayState gstate }
      where playerChangeNextDirection dir player = player { playerNextDirection = dir }
inputKey _ gstate = gstate 

updateGameState :: GameState -> GameState -- Update player's direction, location, and ghosts' location
updateGameState gstate = movePlayer (gstate 
  {infoToShow = ShowGame (level gstate) (player gstate)}
  {player = playerChangeDirection (level gstate) (player gstate)})

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

move :: Player -> Player          -- Change location based on the direction 
move player = case playerDirection player of 
  West -> player { playerLocation = (x - 1, y) }
  East -> player { playerLocation = (x + 1, y) }
  North -> player { playerLocation = (x, y + 1) }
  South -> player { playerLocation = (x, y - 1) }
  where (x, y) = playerLocation player

changePlayState :: GameState -> PlayState           -- Changes the state from begin to playing but immediately back to paused
changePlayState gstate = case playState gstate of 
  Begin     -> Playing
  Playing   -> Paused
  Paused    -> Playing
  GameOver  -> Begin