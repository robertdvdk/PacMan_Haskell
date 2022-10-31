-- | This module contains the data types
--   which represent the state of the game
module Model where

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char
                | ShowAPlayer Player
                | ShowGame Level Player

data Player = Player {
  playerLocation :: Location,
  playerDirection :: Direction,
  playerNextDirection :: NextDirection,
  playerNumLives :: NumLives}

instance Show Player where
  show (Player _ _ _ _) = "vet cool"

type Location = (XCoordinate, YCoordinate)
type XCoordinate = Float
type YCoordinate = Float
type NumLives = Int

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 0.01

data GameState = GameState {
  infoToShow :: InfoToShow,
  elapsedTime :: Float,
  playState :: PlayState,
  player :: Player,
  level :: Level}

type Level = [Location]

data PlayState = Begin | Playing | Paused | GameOver
data Direction = North | South | West | East
type NextDirection = Direction

data Ghost = Ghost {
  ghostLocation :: Location,
  ghostDirection :: Direction,
  ghostNextDirection :: NextDirection,
  ghostColor :: Color,
  ghostEatable :: Eatable,
  ghostPlayerControlled :: PlayerControlled}

data PlayerControlled = IsPlayerControlled | NotPlayerControlled
data Eatable = IsEatable | NotEatable
data Color = Red | Pink | Yellow | Blue


initialState :: GameState
-- initialState = GameState ShowNothing 0 Begin (Player (50, 50) East West 3) []
-- level1 = [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (5, 1), (5, 2), (5, 3), (5, 4), (5, 5)]
makeLevelRectangle :: Location -> Location -> Level
makeLevelRectangle (x1, y1) (x2, y2) = [(x, y) | x <- [x1..x2], y <- [y1, y2]] ++ [(x, y) | x <- [x1, x2], y <- [y1..y2]]
level1 = makeLevelRectangle (-18, -18) (18, 18) ++ makeLevelRectangle (-5, 12) (5, 18) ++ makeLevelRectangle (-16, 0) (-7, 16) ++ makeLevelRectangle (7, 7) (16, 16) ++ makeLevelRectangle (-5, 0) (5, 10) ++ makeLevelRectangle (-16, -6) (5, -2) ++ makeLevelRectangle (7, -6) (16, 5) ++ makeLevelRectangle (-16, -8) (16, -8) ++ makeLevelRectangle (-16, -12) (0, -10) ++ makeLevelRectangle (2, -12) (16, -10) ++ makeLevelRectangle (-16, -16) (-2, -14) ++ makeLevelRectangle (0, -16) (16, -14)
--  ++ makeLevelRectangle (-150, 0) (-80, 150) ++ makeLevelRectangle (80, 80) (150, 150) ++ makeLevelRectangle (-50, 0) (50, 100) ++ makeLevelRectangle (-150, -60) (50, -30) ++ makeLevelRectangle (80, -60) (150, 50) ++ makeLevelRectangle (-150, -90) (150, -90) ++ makeLevelRectangle (-150, -120) (0, -120) ++ makeLevelRectangle (30, -120) (150, -120) ++ makeLevelRectangle (-150, -150) (-30, -150) ++ makeLevelRectangle (0, -150) (150, -150)

-- level1 = [(0,1),(-9,-11),(-9,-10),(-9,-9),(-9,-8),(-9,-7),(-9,-6),(-9,-5),(-9,-4),(-9,-3),(-9,-1),(-9,1),(-9,3),(-9,4),(-9,5),(-9,6),(-9,7),(-9,8),(-9,9),(9,-10),(9,-9),(9,-8),(9,-7),(9,-6),(9,-5),(9,-4),(9,-3),(9,-1),(9,1),(9,3),(9,4),(9,5),(9,6),(9,7),(9,8),(9,9),(-9,-11),(-8,-11),(-7,-11),(-6,-11),(-5,-11),(-4,-11),(-3,-11),(-2,-11),(-1,-11),(0,-11),(1,-11),(2,-11),(3,-11),(4,-11),(5,-11),(6,-11),(7,-11),(8,-11),(9,-11),(-9,9),(-8,9),(-7,9),(-6,9),(-5,9),(-4,9),(-3,9),(-2,9),(-1,9),(0,9),(1,9),(2,9),(3,9),(4,9),(5,9),(6,9),(7,9),(8,9),(9,9),(-2,0),(2,0),(1,1),(-1,1),(2,1),(-2,1),(-2,-1),(-1,-1),(0,-1),(1,-1),(2,-1),(0,3),(0,4),(0,5),(0,7),(0,8),(0,-3),(0,-4),(0,-5),(0,-7),(0,-8),(0,-9),(1,5),(1,-3),(1,-7),(2,3),(2,5),(2,7),(2,-3),(2,-5),(2,-7),(2,-9),(3,3),(3,7),(3,-5),(3,-9),(4,1),(4,2),(4,3),(4,4),(4,5),(4,7),(4,-1),(4,-2),(4,-3),(4,-5),(4,-7),(4,-8),(4,-9),(5,-9),(6,1),(6,2),(6,3),(6,5),(6,7),(6,-1),(6,-2),(6,-3),(6,-5),(6,-6),(6,-7),(6,-9),(7,1),(7,3),(7,5),(7,7),(7,-1),(7,-3),(7,-5),(7,-9),(8,1),(8,3),(8,-1),(8,-3),(8,-7),(10,1),(10,-1),(2,0),(-2,0),(-1,1),(1,1),(-2,1),(2,1),(2,-1),(1,-1),(0,-1),(-1,-1),(-2,-1),(0,3),(0,4),(0,5),(0,7),(0,8),(0,-3),(0,-4),(0,-5),(0,-7),(0,-8),(0,-9),(-1,5),(-1,-3),(-1,-7),(-2,3),(-2,5),(-2,7),(-2,-3),(-2,-5),(-2,-7),(-2,-9),(-3,3),(-3,7),(-3,-5),(-3,-9),(-4,1),(-4,2),(-4,3),(-4,4),(-4,5),(-4,7),(-4,-1),(-4,-2),(-4,-3),(-4,-5),(-4,-7),(-4,-8),(-4,-9),(-5,-9),(-6,1),(-6,2),(-6,3),(-6,5),(-6,7),(-6,-1),(-6,-2),(-6,-3),(-6,-5),(-6,-6),(-6,-7),(-6,-9),(-7,1),(-7,3),(-7,5),(-7,7),(-7,-1),(-7,-3),(-7,-5),(-7,-9),(-8,1),(-8,3),(-8,-1),(-8,-3),(-8,-7),(-10,1),(-10,-1)]



initialState = GameState ShowNothing 0 Begin (Player (0, -2) North North 3) level1
