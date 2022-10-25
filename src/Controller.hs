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
  = -- We show a new random number
    do return $ movePlayer (GameState (ShowAPlayer (getPlayer gstate)) 0 Playing (getPlayer gstate))
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate
  -- If the user presses a character key, show that one
    | c == 'w'    = gstate { player = playerChangeDirection North (getPlayer gstate)}
    | c == 's'    = gstate { player = playerChangeDirection South (getPlayer gstate)}
    | c == 'a'    = gstate { player = playerChangeDirection West (getPlayer gstate)}
    | c == 'd'    = gstate { player = playerChangeDirection East (getPlayer gstate)}
    | otherwise   = gstate { infoToShow = ShowAChar c }
inputKey _ gstate = gstate -- Otherwise keep the same

pauseGame :: GameState -> IO GameState
pauseGame gstate = do c <- getChar
                      case c of
                            ' ' -> changePauseState gstate where
                              changePauseState (GameState a b Playing c) = return $ GameState a b Paused c
                              changePauseState (GameState a b Paused c) = return $ GameState a b Playing c

movePlayer :: GameState -> GameState
movePlayer (GameState a b c player) = GameState a 0 c (move player)

move :: Player -> Player
move (Player (x, y) dir nextdir lives) = case dir of 
  West -> Player (x - 5, y) dir nextdir lives
  East -> Player (x + 5, y) dir nextdir lives
  North -> Player (x, y + 5) dir nextdir lives
  South -> Player (x, y - 5) dir nextdir lives

getPlayer :: GameState -> Player
getPlayer (GameState a b c player) = player

playerChangeDirection :: Direction -> Player -> Player
playerChangeDirection dir (Player loc playerdir playernextdir lives) = Player loc dir playernextdir lives