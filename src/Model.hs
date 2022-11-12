-- | This module contains the data types
-- | which represent the state of the game
module Model where

import Graphics.Gloss
import Data.List

-- | The Gamestate Model
data GameState = GameState {
  playState   :: PlayState,
  player      :: Player,
  levels      :: [Level],
  score       :: Score,
  highScores  :: [Int]
}

data PlayState  = Start | Playing | Paused | GameOver | Win
type Score      = Int

-- | The Player Model
data Player = Player {
  pacManBitMaps       :: [Picture],
  playerLocation      :: Location,
  playerDirection     :: Direction,
  playerNextDirection :: NextDirection,
  dyingTimer          :: Float
}

type Location       = (Float, Float)
data Direction      = North | South | West | East deriving Eq
type NextDirection  = Direction

-- | The level model
data Level = Level {
  levelColor  :: Color,
  maze        :: Maze,
  ghostCage   :: Cage,
  ghosts      :: [Ghost],
  food        :: Food,
  largeFood   :: LargeFood,
  playerSpawn :: PlayerSpawn,
  cageTimer   :: Float
}

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
  ghostNextDirection  :: NextDirection,
  ghostOutsideCage    :: GhostOutsideCage,
  difficulty          :: Int
}

data GhostOutsideCage = InsideCage | OutsideCage
data GhostColor       = Red | Pink | Yellow | Blue
data Eatable          = IsEatable | NotEatable

class Entity a where
  entityLocation  :: a -> Location
  entityDirection :: a -> Direction

instance Entity Ghost where
  entityLocation  = ghostLocation
  entityDirection = ghostDirection

instance Entity Player where
  entityLocation  = playerLocation
  entityDirection = playerDirection