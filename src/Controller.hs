-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import ReadWrite

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = case playState gstate of
      Begin     -> do 
                      highScores <- readF
                      return $ initialState { highScores = highScores}
      Playing   -> updateGameState gstate
      GameOver  -> do   
                      writeF gstate
                      return $ gstate
      _         -> return $ gstate 

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

-- | Update player's direction, location, and ghosts' location
updateGameState :: GameState -> IO GameState
updateGameState gstate = do let gstate' = movePlayer (gstate {player = playerChangeDirection (maze (level gstate)) (player gstate)})
                            let gstate'' = flashCage gstate'
                            let gstate''' = gstate'' {ghost1 = checkGhostInCage (ghostCage (level gstate'')) (ghost1 gstate'')}
                            moveGhost gstate''

-- | Check if there is a wall in the given direction. If there is not, keep moving. If there is, choose a random next direction.
moveGhost :: GameState -> IO GameState
moveGhost gstate = do chosendir <- ghostPickNextDirection (maze (level gstate)) (ghost1 gstate)
                      if noWall (ghostOutsideCage (ghost1 gstate)) then 
                        return gstate {ghost1 = ghostChangeLocation (ghost1 gstate)} else
                          return gstate {ghost1 = ghostChangeDirection (maze (level gstate)) (ghost1 gstate) (chosendir)}
  where noWall InsideCage = cageInDirection (ghostCage (level gstate)) (ghostDirection (ghost1 gstate)) (ghostLocation (ghost1 gstate)) || (not (wallInDirection (maze (level gstate)) (ghostDirection (ghost1 gstate)) (ghostLocation (ghost1 gstate))))
        noWall OutsideCage = not (wallInDirection (maze (level gstate)) (ghostDirection (ghost1 gstate)) (ghostLocation (ghost1 gstate)))

-- | Generates a random int between x and y
getInt :: Int -> Int -> IO Int
getInt x y = getStdRandom (randomR (x, y))

-- | Picks a random direction for a ghost
ghostPickNextDirection :: Maze -> Ghost -> IO Direction
ghostPickNextDirection maze ghost = do let xs = [a | a <- [North, East, South, West], ghostAbleToChangeDirection maze ghost a]
                                       dir <- getInt 0 (length xs - 1)
                                       return (xs !! dir)

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

-- | Increments the number of frames for the cage; the cages flips between yellow and blue for 5 frames each
flashCage :: GameState -> GameState
flashCage gstate | frames gstate == 10 = gstate {frames = 0}
                 | otherwise = gstate {frames = (frames gstate + 1)}

 -- | Checks if Pac-Man can make a turn
playerAbleToChangeDirection :: Maze -> Player -> Bool 
playerAbleToChangeDirection maze player = not (wallInDirection maze (playerNextDirection player) (playerLocation player))

-- | If Pac-Man can turn towards the input direction, then do so
playerChangeDirection :: Maze -> Player -> Player
playerChangeDirection maze player  | playerAbleToChangeDirection maze player = player { playerDirection = playerNextDirection player}  
                                   | otherwise = player

lockGhostOutsideCage :: GameState -> GameState
lockGhostOutsideCage gstate = gstate {ghost1 = (ghost1 gstate) {ghostOutsideCage = OutsideCage}}

-- | Checks if the ghost can turn towards the given direction
ghostAbleToChangeDirection :: Maze -> Ghost -> Direction -> Bool
ghostAbleToChangeDirection maze ghost dir = case ghostOutsideCage ghost of 
  InsideCage -> cageInDirection maze dir (ghostLocation ghost) || not (wallInDirection maze dir (ghostLocation ghost))
  OutsideCage -> not (wallInDirection maze dir (ghostLocation ghost))

ghostChangeDirection :: Maze -> Ghost -> Direction -> Ghost
ghostChangeDirection maze ghost dir | ghostAbleToChangeDirection maze ghost dir = ghost {ghostDirection = dir}
                                    | otherwise = ghost

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

-- | Check if there is a 'cage wall' in the given direction: ghosts should only be able to move through this wall one-way.
cageInDirection :: Cage -> Direction -> Location -> Bool
cageInDirection cage dir (x, y) = case dir of
  West  -> (x - 1, y) `elem` cage
  East  -> (x + 1, y) `elem` cage
  North -> (x, y + 1) `elem` cage
  South -> (x, y - 1) `elem` cage

-- | Change location based on the direction 
playerChangeLocation :: Player -> Player
playerChangeLocation player = case playerDirection player of 
  West  -> player { playerLocation = (x - 1, y) }
  East  -> player { playerLocation = (x + 1, y) }
  North -> player { playerLocation = (x, y + 1) }
  South -> player { playerLocation = (x, y - 1) }
  where (x, y) = playerLocation player


-- TODO: hoe zou dit handiger kunnen? Liever geen code dupliceren. -- het kan net iets beter maar dit lijkt me prima, is erg overzichtelijk.
-- zie volgende voorbeeld hoe het ook kan. maar vind ik minder overzichtelijk.

-- ghostChangeLocation :: Ghost -> Ghost
-- ghostChangeLocation ghost = case ghostDirection ghost of
--   West  -> functie (x - 1, y)
--   East  -> functie (x + 1, y)
--   North -> functie (x, y + 1)
--   South -> functie (x, y - 1)
--   where functie (x, y) = ghost { ghostLocation = (x, y) }
--   where (x, y) = ghostLocation ghost

ghostChangeLocation :: Ghost -> Ghost
ghostChangeLocation ghost = case ghostDirection ghost of
  West  -> ghost { ghostLocation = (x - 1, y) }
  East  -> ghost { ghostLocation = (x + 1, y) }
  North -> ghost { ghostLocation = (x, y + 1) }
  South -> ghost { ghostLocation = (x, y - 1) }
  where (x, y) = ghostLocation ghost

-- | Changes the state based on the current state and pressed key
changePlayState :: GameState -> PlayState 
changePlayState gstate = case playState gstate of 
  Begin     -> Playing
  Playing   -> Paused
  Paused    -> Playing
  GameOver  -> Begin

-- | Check if the ghost is passing through the cage
checkGhostInCage :: Cage -> Ghost -> Ghost
checkGhostInCage cage ghost | (ghostLocation ghost) `elem` cage = ghost {ghostOutsideCage = OutsideCage}
                            | otherwise = ghost