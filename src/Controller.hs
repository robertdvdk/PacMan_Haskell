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
      Playing -> if elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES 
        then return $ updateGameState (gstate {infoToShow = ShowGame (level gstate) (player gstate)})
        else return $ gstate { elapsedTime = elapsedTime gstate + secs }
      _       -> return $ gstate { elapsedTime = elapsedTime gstate + secs }

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
    | c == 'p'    = changePlayState gstate
      where playerChangeNextDirection dir player = player { playerNextDirection = dir }
inputKey _ gstate = gstate 

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
  West  -> any (==True) [(x, v) `elem` level | x <- [u - 10 .. u]]
  East  -> any (==True) [(x, v) `elem` level | x <- [u .. u + 10]]
  North -> any (==True) [(u, y) `elem` level | y <- [v .. v + 10]]
  South -> any (==True) [(u, y) `elem` level | y <- [v - 10 .. v]]
  where (u, v) = playerLocation player

updateGameState :: GameState -> GameState -- Update player's direction, location, and ghosts' location
updateGameState gstate = movePlayer (gstate {player = playerChangeDirection (level gstate) (player gstate)})

move :: Player -> Player          -- Change location based on the direction 
move player = case playerDirection player of 
  West -> player { playerLocation = (x - 5, y) }
  East -> player { playerLocation = (x + 5, y) }
  North -> player { playerLocation = (x, y + 5) }
  South -> player { playerLocation = (x, y - 5) }
  where (x, y) = playerLocation player

changePlayState :: GameState -> GameState           -- Changes the state from begin to playing but immediately back to paused
changePlayState gstate = case playState gstate of 
        Begin     -> gstate { playState = Playing }
        Playing   -> gstate { playState = Paused }
        Paused    -> gstate { playState = Playing }
        GameOver  -> gstate { playState = Begin }