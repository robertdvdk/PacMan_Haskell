-- | This module defines how the entities move
-- | in response to time and user input
module Movement where

import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import ReadWrite
import Data.List

-- | Check if a player either collided with a ghost this frame, or will collide in the next frame. If so, it's game over.
checkPlayerGhostCollision :: GameState -> Bool
checkPlayerGhostCollision gstate = checkPlayerGhostCollision' (player gstate) (ghost1 gstate) 
  where checkPlayerGhostCollision' player ghost = (checkPlayerGhostCollision'' (playerLocation player) (ghostLocation ghost)) || (checkPlayerGhostCollision'' (playerLocation (playerChangeLocation player)) (ghostLocation ghost))
          where  checkPlayerGhostCollision'' (x1, y1) (x2, y2) = (x1 == x2 && y1 == y2)

-- | First check if the ghost is inside the cage. If it is, then it can move through the cage. 
-- | If it is already outside the cage, it can't go back in. 
moveGhost :: GameState -> IO GameState
moveGhost gstate = do 
  if noWall (ghostOutsideCage gsGhost1) 
    then case ghostOutsideCage gsGhost1 of
          InsideCage -> return gstate {ghost1 = ghostChangeLocation gsGhost1}
          -- If the ghost is outside the cage and is at an intersection or T-junction, choose a random next direction
          OutsideCage ->  if length [a | a <- [North, East, South, West], ghostAbleToChangeDirection gsLevel gsGhost1 a] > 2 
                            then do chosendir <- ghostPickWeightedNextDirection gsLevel gsGhost1 gsPlayer 10
                                    return gstate {ghost1 = ghostChangeLocation (ghostChangeDirection gsLevel gsGhost1 (chosendir))} 
                            -- If the ghost is not at an intersection, simply keep moving
                            else return gstate {ghost1 = ghostChangeLocation gsGhost1}
    -- If the ghost walks into a wall, choose a random next direction
    else do chosendir <- ghostPickWeightedNextDirection gsLevel gsGhost1 gsPlayer 10
            return gstate {ghost1 = ghostChangeDirection gsLevel gsGhost1 (chosendir)}
  where noWall InsideCage = wallInDirection (ghostCage gsLevel) (ghostDirection gsGhost1) (ghostLocation gsGhost1) || (not (wallInDirection (maze gsLevel) (ghostDirection gsGhost1) (ghostLocation gsGhost1)))
        noWall OutsideCage = not (wallInDirection (maze gsLevel) (ghostDirection gsGhost1) (ghostLocation gsGhost1))
        gsGhost1  = ghost1 gstate
        gsLevel   = level gstate
        gsPlayer  = player gstate

-- | Generates a random int between x and y
getInt :: Int -> Int -> IO Int
getInt x y = getStdRandom (randomR (x, y))

-- | Picks a random direction for a ghost
ghostPickNextDirection :: Level -> Ghost -> IO Direction
ghostPickNextDirection level ghost = do let xs = [a | a <- [North, East, South, West], ghostAbleToChangeDirection level ghost a]
                                        dir <- getInt 0 (length xs - 1)
                                        return (xs !! dir)

ghostPickWeightedNextDirection :: Level -> Ghost -> Player -> Int -> IO Direction
ghostPickWeightedNextDirection level ghost player weightfactor = 
    do  let xs = ghostGenerateWeightedNextDirections level ghost player weightfactor
        dir <- getInt 0 (length xs - 1)
        return (xs !! dir)

ghostGenerateWeightedNextDirections :: Level -> Ghost -> Player -> Int -> [Direction]
ghostGenerateWeightedNextDirections level ghost player weightfactor 
    | dx < 0 && dy < 0 = weightDir East (weightDir North xs (-dy*weightfactor)) (-dx*weightfactor)
    | dx < 0 && dy > 0 = weightDir East (weightDir South xs (dy*weightfactor)) (-dx*weightfactor)
    | dx > 0 && dy < 0 = weightDir West (weightDir North xs (-dy*weightfactor)) (dx*weightfactor)
    | dx > 0 && dy > 0 = weightDir West (weightDir South xs (dy*weightfactor)) (dx*weightfactor)
    | dx < 0 = weightDir East xs (-dx*weightfactor)
    | dx > 0 = weightDir West xs (dx*weightfactor)
    | dy < 0 = weightDir North xs (-dy*weightfactor)
    | dy > 0 = weightDir South xs (dy*weightfactor)
    | otherwise = xs
  where (px, py) = playerLocation player
        (gx, gy) = ghostLocation ghost
        (dx, dy) = (round (gx - px), round (gy - py))
        xs = [a | a <- [North, East, South, West], ghostAbleToChangeDirection level ghost a]
        weightDir :: Direction -> [Direction] -> Int -> [Direction]
        weightDir newDir dirs 0 = dirs
        weightDir newDir dirs weight = if newDir `elem` dirs then (newDir : (weightDir newDir dirs (weight - 1))) else dirs

-- | Increments the number of frames for the cage; the cages flips between yellow and blue for 5 frames each
flashCage :: GameState -> GameState
flashCage gstate | frames gstate == 10 = gstate {frames = 0}
                 | otherwise = gstate {frames = (frames gstate + 1)}

eatFood :: GameState -> GameState
eatFood gstate | playerLocation (player gstate) `elem` food (level gstate) = gstate {score = (score gstate + 10), level = (removeFood (level gstate))}
               | playerLocation (player gstate) `elem` largeFood (level gstate) = gstate {score = (score gstate + 50), level = (removeLargeFood (level gstate))}
               | otherwise = gstate
  where removeFood :: Level -> Level
        removeFood level = level {food = delete (playerLocation (player gstate)) (food level)}
        removeLargeFood :: Level -> Level
        removeLargeFood level = level {largeFood = delete (playerLocation (player gstate)) (largeFood level)}

-- | Checks if Pac-Man can make a turn
playerAbleToChangeDirection :: Maze -> Player -> Bool 
playerAbleToChangeDirection maze player = not (wallInDirection maze (playerNextDirection player) (playerLocation player))

-- | If Pac-Man can turn towards the input direction, then do so
playerChangeDirection :: Maze -> Player -> Player
playerChangeDirection maze player  | playerAbleToChangeDirection maze player = player { playerDirection = playerNextDirection player}  
                                   | otherwise = player

-- | If ghost can turn towards chosen direction, then do so
ghostChangeDirection :: Level -> Ghost -> Direction -> Ghost
ghostChangeDirection level ghost dir | ghostAbleToChangeDirection level ghost dir = ghost {ghostDirection = dir}
                                     | otherwise = ghost

lockGhostOutsideCage :: GameState -> GameState
lockGhostOutsideCage gstate = gstate {ghost1 = (ghost1 gstate) {ghostOutsideCage = OutsideCage}}

-- | Checks if the ghost can turn towards the given direction
-- | If the ghost is still inside the cage, they may move through the cage wall. Otherwise, they may not.
ghostAbleToChangeDirection :: Level -> Ghost -> Direction -> Bool
ghostAbleToChangeDirection level ghost dir = case ghostOutsideCage ghost of 
  InsideCage -> wallInDirection (ghostCage level) dir (ghostLocation ghost) || not (wallInDirection (maze level) dir (ghostLocation ghost))
  OutsideCage -> not (wallInDirection (maze level) dir (ghostLocation ghost))


movePlayer :: GameState -> GameState  
movePlayer gstate | noWall = gstate { player = playerChangeLocation (player gstate)}
                  | otherwise = gstate
  where noWall = not (wallInDirection (maze (level gstate)) (playerDirection (player gstate)) (playerLocation (player gstate)))

-- | Check if there is a wall in the given direction: checks the point in the direction towards which the player is travelling
-- | and checks if that point is in the 'level' list that contains the walls
wallInDirection :: Maze -> Direction -> Location -> Bool
wallInDirection maze dir (x, y) = case dir of
  West  -> (x - 1, y) `elem` maze
  East  -> (x + 1, y) `elem` maze
  North -> (x, y + 1) `elem` maze
  South -> (x, y - 1) `elem` maze

-- | Gives back the new location of an entity, given its current location and the direction it's heading
entityNewLocation :: Entity a => a -> Location
entityNewLocation entity = case entityDirection entity of
  West  -> (x-1, y)
  East  -> (x+1, y)
  North -> (x, y+1)
  South -> (x, y-1)
  where (x, y) = entityLocation entity

-- | Change location of player based on player's direction 
playerChangeLocation :: Player -> Player
playerChangeLocation player = player {playerLocation = entityNewLocation player}

-- | Change location of ghost based on ghost's direction
ghostChangeLocation :: Ghost -> Ghost
ghostChangeLocation ghost = ghost {ghostLocation = entityNewLocation ghost}

-- | Check if the ghost is passing through the cage
checkGhostInCage :: Cage -> Ghost -> Ghost
checkGhostInCage cage ghost | (ghostLocation ghost) `elem` cage = ghost {ghostOutsideCage = OutsideCage}
                            | otherwise = ghost

