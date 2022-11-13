-- | This module contains the data types
-- | which represent the state of the game
module Model where

import Graphics.Gloss
import Data.List

-- | The Gamestate Model
data GameState = GameState {
  playState     	    :: PlayState,
  player              :: Player,
  level               :: Level,
  levels              :: [Level],
  score               :: Score,
  highScores          :: [Int],
  levelCounter        :: Int,
  eatableGhostBitMap  :: Picture
}

data PlayState  = Start | Playing | Paused | GameOver | Win | Won | WonEntireGame
type Score      = Int

-- | The Player Model
data Player = Player {
  pacManBitMaps       :: [Picture],
  playerLocation      :: Location,
  playerDirection     :: Direction,
  playerNextDirection :: NextDirection,
  dyingTimer          :: Float,
  playerLives         :: Lives
}
type Lives          = Int
type Location       = (Float, Float)
data Direction      = North | South | West | East deriving Eq
type NextDirection  = Direction

-- | The level model
data Level = Level {
  levelColor    :: Color,
  maze          :: Maze,
  ghostCage     :: Cage,
  ghosts        :: [Ghost],
  food          :: Food,
  largeFood     :: LargeFood,
  playerSpawn   :: PlayerSpawn,
  ghostsSpawn   :: [GhostSpawn],
  cageTimer     :: Float,
  ghostsEatable :: (IsEatable, Float)
}
data IsEatable    = NotEatable | Eatable deriving Show
type Maze         = [Location]
type Cage         = [Location]
type Food         = [Location]
type LargeFood    = [Location]
type PlayerSpawn  = Location
type GhostSpawn   = Location

-- | The Ghost Model
data Ghost = Ghost {
  ghostBitMap         :: Picture,
  ghostLocation       :: Location,
  ghostDirection      :: Direction,
  ghostOutsideCage    :: GhostOutsideCage,
  difficulty          :: Int,
  ghostIndex          :: Int
}


data GhostOutsideCage = InsideCage | OutsideCage
data GhostColor       = Red | Pink | Yellow | Blue

class Entity a where
  entityLocation  :: a -> Location
  entityDirection :: a -> Direction

instance Entity Ghost where
  entityLocation  = ghostLocation
  entityDirection = ghostDirection

instance Entity Player where
  entityLocation  = playerLocation
  entityDirection = playerDirection