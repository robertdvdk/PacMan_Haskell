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
level1 = makeLevelRectangle (-180, -180) (180, 180) ++ makeLevelRectangle (-50, 130) (50, 180) ++ makeLevelRectangle (-150, 0) (-80, 150) ++ makeLevelRectangle (80, 80) (150, 150) ++ makeLevelRectangle (-50, 0) (50, 100) ++ makeLevelRectangle (-150, -60) (50, -30) ++ makeLevelRectangle (80, -60) (150, 50) ++ makeLevelRectangle (-150, -90) (150, -90) ++ makeLevelRectangle (-150, -120) (0, -120) ++ makeLevelRectangle (30, -120) (150, -120) ++ makeLevelRectangle (-150, -150) (-30, -150) ++ makeLevelRectangle (0, -150) (150, -150)
initialState = GameState ShowNothing 0 Begin (Player (0, -20) North North 3) level1
