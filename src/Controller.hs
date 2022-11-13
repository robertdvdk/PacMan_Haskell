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
                      scores  <- readF
                      return $ resetLevel gstate {highScores = scores}-- { level = resetLevel (level gstate) (levels gstate), player = (player gstate) { dyingTimer = 0 }, highScores = highScores }
      Playing   -> updateGameState gstate
      GameOver  -> do   
                      writeF gstate
                      dyingAnimation gstate

      Win       -> do if levelCounter gstate == 3 then return $ gstate {playState = WonEntireGame} else return $ gstate -- {level = nextLevel (level gstate) (levels gstate), player = (player gstate) {dyingTimer = 0}, playState = Playing}
      Won       -> do return $ (nextLevel gstate) {playState = Start}
      _         -> do return gstate

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

-- | Animate Pac-Man's death when he has lost all 3 lives
dyingAnimation :: GameState -> IO GameState
dyingAnimation gstate = 
  do 
    let gstate' = gstate { player = setTimer (player gstate) }
    let gstate'' = gstate' { level = (level gstate') { ghosts = resetGhosts (ghostsSpawn (level gstate')) (ghosts (level gstate'))}}
    return gstate''

-- | Update player's direction, location, and ghosts' location
-- | This should probably be done using the State monad, but this was not done due to lack of time
updateGameState :: GameState -> IO GameState
updateGameState gstate = 
  do 
    let gstate'     = movePlayer (gstate { player = playerChangeDirection (maze (level gstate)) (player gstate) })
    let gstate''    = eatFood gstate'
    let gstate'''   = gstate''  { level = flashCage (level gstate'') }
    let gstate''''  = gstate''' { level = (level gstate''') { ghosts = checkGhostInCage (ghostCage (level gstate''')) (ghosts (level gstate''')) } }
    movedGhosts <- moveGhost gstate'''' (ghosts (level gstate''''))
    let gstate''''' = gstate'''' { level = (level gstate'''') { ghosts = movedGhosts } }
    let gstate'''''' = checkPlayerGhostCollision gstate''''' (ghosts (level gstate'''''))
    let gstate'''''''   = checkEverythingEaten gstate''''''
    let gstate''''''''   = updateEatable gstate'''''''
    return gstate''''''''


-- | PURE PART STARTS HERE
-- | Set ghosts to their spawn locations
resetGhosts :: [GhostSpawn] -> [Ghost] -> [Ghost]
resetGhosts _ [] = []
resetGhosts [] _ = []
resetGhosts (s:ss) (g:gs) = g {ghostLocation = s} : resetGhosts ss gs

-- | Increments the number of frames for the dying animation
setTimer :: Player -> Player
setTimer player = player { dyingTimer = (dyingTimer player + 1) }

-- | Check if a player either collided with a ghost this frame, or will collide in the next frame. If there was a collision, the player will lose a life,
-- | unless the ghosts are currently eatable, in which case the ghost will disappear.
checkPlayerGhostCollision :: GameState -> [Ghost] -> GameState
checkPlayerGhostCollision gstate gs = case checkPlayerGhostCollision' gstate gs of
  Nothing -> gstate
  Just a -> case ghostsEatable (level gstate) of
    (Eatable, _) -> gstate {level = deleteGhost (level gstate) a} 
    (NotEatable, _) -> decrementLives gstate 
  where
    checkPlayerGhostCollision' gstate [] = Nothing
    checkPlayerGhostCollision' gstate gs = elemIndex True [checkPlayerGhostCollision'' (player gstate) g | g <- gs]
    checkPlayerGhostCollision'' player g  = 
          (sameLocation (playerLocation player) (ghostLocation g)) || (sameLocation (playerLocation (playerChangeLocation player)) (ghostLocation g))
                where  sameLocation (x1, y1) (x2, y2) = (x1 == x2 && y1 == y2)

decrementLives :: GameState -> GameState
decrementLives gstate = case playerLives (player gstate) of
  1 -> gstate {playState = GameOver}
  _ -> gstate {player = dec (player gstate), playState = Start}
  where
    dec player = player {playerLives = playerLives player - 1}

deleteGhost :: Level -> Int -> Level
deleteGhost lv n = let (as,b:bs) = splitAt n (ghosts lv) in lv {ghosts = as ++ bs}

updateEatable :: GameState -> GameState
updateEatable gstate = gstate {level = decrementEatable (level gstate)} where
  decrementEatable lv = case ghostsEatable lv of
    (Eatable, 0) -> lv {ghostsEatable = (NotEatable, 0)}
    (Eatable, x) -> lv {ghostsEatable = (Eatable, x-1)}
    _              -> lv

-- | Increments the number of frames for the cage; the cages flips between yellow and blue for 5 frames each
flashCage :: Level -> Level
flashCage level | cageTimer level == 10  = level { cageTimer = 0}
                | otherwise              = level { cageTimer = (cageTimer level + 1) }
                            
inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) Down _ _) gstate
    | c == 'w'    = changeDirection North
    | c == 's'    = changeDirection South
    | c == 'a'    = changeDirection West
    | c == 'd'    = changeDirection East
    | c == 'p'    = changeGameState gstate
    | c == 'g'    = gstate { playState = GameOver }                                   -- TEST GAMEOVER
    | c == 't'    = gstate { playState = Win }                                        -- TEST WIN
    where   [level1, level2, level3] = levels gstate
            changeDirection direction = gstate { player = playerChangeNextDirection direction (player gstate) }
              where playerChangeNextDirection dir player = player { playerNextDirection = dir }
inputKey _ gstate = gstate

-- | Changes the state based on the current state and pressed key
changeGameState :: GameState -> GameState 
changeGameState gstate = case playState gstate of 
  Start         -> gstate { playState = Playing }
  Playing       -> gstate { playState = Paused }
  Paused        -> gstate { playState = Playing }
  GameOver      -> gstate { playState = Start, score = 0, levelCounter = 1, player = (player gstate) {playerLives = 3}}
  Win           -> gstate { playState = Won }
  WonEntireGame -> gstate { playState = Won }

-- | Resets the current to its initial state
resetLevel :: GameState -> GameState
resetLevel gstate = case levelCounter gstate of
  1 -> initializeLevel gstate level1
  2 -> initializeLevel gstate level2
  3 -> initializeLevel gstate level3
  where [level1, level2, level3] = levels gstate

-- | Changes to the next level. Level 3 loops back around to level 1
nextLevel :: GameState -> GameState
nextLevel gstate = case levelCounter gstate of 
  1 -> (initializeLevel gstate level2) { levelCounter = 2 }
  2 -> (initializeLevel gstate level3) { levelCounter = 3 }
  3 -> (initializeLevel gstate level1) { levelCounter = 1 }
  where [level1, level2, level3] = levels gstate

-- | Initializes a given level.
initializeLevel :: GameState -> Level -> GameState
initializeLevel gstate newlevel = gstate {player = newplayer, level = initlevel}
  where
    newplayer = (player gstate) { playerLocation = playerSpawn newlevel, playerDirection = West, dyingTimer = 0 }
    initlevel = newlevel { ghosts = resetGhosts (ghostsSpawn newlevel) (ghosts newlevel) }

-- | Checks if the player has eaten all the food in the maze.
checkEverythingEaten :: GameState -> GameState
checkEverythingEaten gstate = if checkEverythingEaten' gstate then gstate {playState = Win} else gstate where
  checkEverythingEaten' gstate = length (food (level gstate)) == 0 && length (largeFood (level gstate)) == 0
