-- | This module defines how the entities move
-- | in response to time and user input
module Movement where

import Model
import System.Random
import Data.List

checkEverythingEaten :: GameState -> Bool -- Needs to be made
checkEverythingEaten gstate = False

-- | Check if a player either collided with a ghost this frame, or will collide in the next frame. If so, it's game over.
checkPlayerGhostCollision :: GameState -> [Ghost] -> Bool
checkPlayerGhostCollision gstate [] = False
checkPlayerGhostCollision gstate gs = True `elem` [checkPlayerGhostCollision' (player gstate) g | g <- gs]
  where checkPlayerGhostCollision' player g = case g of 
          None -> False
          _ -> (checkPlayerGhostCollision'' (playerLocation player) (ghostLocation g)) || (checkPlayerGhostCollision'' (playerLocation (playerChangeLocation player)) (ghostLocation g))
                where  checkPlayerGhostCollision'' (x1, y1) (x2, y2) = (x1 == x2 && y1 == y2)

-- | First check if the ghost is inside the cage. If it is, then it can move through the cage. 
-- | If it is already outside the cage, it can't go back in. 
moveGhost :: GameState -> [Ghost] -> IO [Ghost]
moveGhost gstate [] = return []
moveGhost gstate (g:gs) = do 
  case g of
    None -> return [None]
    _ -> if noWall (ghostOutsideCage g) 
          then case ghostOutsideCage g of
                InsideCage -> do moverest <- moveGhost gstate gs
                                 return ((ghostChangeLocation g) : moverest)
                -- If the ghost is outside the cage and is at an intersection or T-junction, choose a random next direction
                OutsideCage ->  if length [a | a <- [North, East, South, West], ghostAbleToChangeDirection gsLevel g a] > 2 
                                  then do chosendir <- ghostPickWeightedNextDirection gsLevel g gsPlayer weight
                                          moverest <- moveGhost gstate gs
                                          return ((ghostChangeLocation (ghostChangeDirection gsLevel g chosendir)) : moverest)
                                  -- If the ghost is not at an intersection, simply keep moving
                                  else do moverest <- moveGhost gstate gs
                                          return ((ghostChangeLocation g) : moverest)
          -- If the ghost walks into a wall, choose a random next direction
          else do chosendir <- ghostPickWeightedNextDirection gsLevel g gsPlayer weight
                  moverest <- moveGhost gstate gs
                  return ((ghostChangeDirection gsLevel g chosendir) : moverest)
  where noWall InsideCage = wallInDirection (ghostCage gsLevel) (ghostDirection g) (ghostLocation g) || (not (wallInDirection (maze gsLevel) (ghostDirection g) (ghostLocation g)))
        noWall OutsideCage = not (wallInDirection (maze gsLevel) (ghostDirection g) (ghostLocation g))
        gsLevel   = level gstate
        gsPlayer  = player gstate
        weight = case ghostColor g of
          Red -> 0
          Pink -> 1
          Yellow -> 3
          Blue -> 10

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
    | dx < 0 && dy < 0 = weightDir East (weightDir North xs (-dy* weightfactor)) (-dx * weightfactor)
    | dx < 0 && dy > 0 = weightDir East (weightDir South xs (dy * weightfactor)) (-dx * weightfactor)
    | dx > 0 && dy < 0 = weightDir West (weightDir North xs (-dy* weightfactor)) (dx  * weightfactor)
    | dx > 0 && dy > 0 = weightDir West (weightDir South xs (dy * weightfactor)) (dx  * weightfactor)
    | dx < 0 = weightDir East   xs (-dx * weightfactor)
    | dx > 0 = weightDir West   xs (dx  * weightfactor)
    | dy < 0 = weightDir North  xs (-dy * weightfactor)
    | dy > 0 = weightDir South  xs (dy  * weightfactor)
    | otherwise = xs
  where (px, py) = playerLocation player
        (gx, gy) = ghostLocation ghost
        (dx, dy) = (round (gx - px), round (gy - py))
        xs = [a | a <- [North, East, South, West], ghostAbleToChangeDirection level ghost a]
        weightDir :: Direction -> [Direction] -> Int -> [Direction]
        weightDir newDir dirs 0 = dirs
        weightDir newDir dirs weight = if newDir `elem` dirs then (newDir : (weightDir newDir dirs (weight - 1))) else dirs

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
playerChangeDirection maze player  | playerAbleToChangeDirection maze player = player { playerDirection = playerNextDirection player }  
                                   | otherwise = player

-- | If ghost can turn towards chosen direction, then do so
ghostChangeDirection :: Level -> Ghost -> Direction -> Ghost
ghostChangeDirection level ghost dir | ghostAbleToChangeDirection level ghost dir = ghost { ghostDirection = dir }
                                     | otherwise = ghost

lockGhostOutsideCage :: Ghost -> Ghost
lockGhostOutsideCage ghost = ghost { ghostOutsideCage = OutsideCage } 

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
checkGhostInCage :: Cage -> [Ghost] -> [Ghost]
checkGhostInCage cage [] = []
checkGhostInCage cage (g:gs) = case g of
  None -> None : checkGhostInCage cage gs
  _ -> if ghostLocation g `elem` cage 
        then g {ghostOutsideCage = OutsideCage} : checkGhostInCage cage gs
        else g : checkGhostInCage cage gs
  
