-- | This module contains the data types
--   which represent the state of the game
module Model where

-- The Gamestate Model
data GameState = GameState {
  playState :: PlayState,
  player    :: Player,
  level     :: Level,
  score     :: Score}

data PlayState  = Begin | Playing | Paused | GameOver
type Level      = [Location]
type Score      = Int

-- The Player Model
data Player = Player {
  playerLocation      :: Location,
  playerDirection     :: Direction,
  playerNextDirection :: NextDirection}

type Location       = (Float, Float)
data Direction      = North | South | West | East
type NextDirection  = Direction

-- The Ghost Model
data Ghost = Ghost {
  ghostLocation       :: Location,
  ghostDirection      :: Direction,
  ghostNextDirection  :: NextDirection,
  ghostColor          :: Color,
  ghostEatable        :: Eatable}

data Color    = Red | Pink | Yellow | Blue
data Eatable  = IsEatable | NotEatable

-- Level and Initial State
makeLevelRectangle :: (Location, Location) -> Level
makeLevelRectangle ((x1, y1), (x2, y2)) = [(x, y) | x <- [x1..x2], y <- [y1, y2]] ++ [(x, y) | x <- [x1, x2], y <- [y1..y2]]

-- level1 = makeLevelRectangle (-180, -180) (180, 180) ++ makeLevelRectangle (-50, 130) (50, 180) ++ makeLevelRectangle (-150, 0) (-80, 150) ++ makeLevelRectangle (80, 80) (150, 150) ++ makeLevelRectangle (-50, 0) (50, 100) ++ makeLevelRectangle (-150, -60) (50, -30) 
-- ++ makeLevelRectangle (80, -60) (150, 50) ++ makeLevelRectangle (-150, -90) (150, -90) ++ makeLevelRectangle (-150, -120) (0, -120) 
-- ++ makeLevelRectangle (30, -120) (150, -120) ++ makeLevelRectangle (-150, -150) (-30, -150) ++ makeLevelRectangle (0, -150) (150, -150)

level1 = concatMap makeLevelRectangle 
  [((-180, 180),  (180, 180)), 
  ((-50, 130),    (50, 180)), 
  ((-150, 0),     (-80, 150)), 
  ((80, 80),      (150, 150)), 
  ((-50, 0),      (50, 100)), 
  ((-150, -60),   (50, -30)), 
  ((80, -60),     (150, 50)), 
  ((-150, -90),   (150, -90)), 
  ((-150, -120),  (0, -120)), 
  ((30, -120),    (150, -120)), 
  ((-150, -150),  (-30, -150)), 
  ((0, -150),     (150, -150))]

initialPlayer :: Player
initialPlayer = Player (0, -20) West West

initialState :: GameState
initialState = GameState Begin initialPlayer level1 1000