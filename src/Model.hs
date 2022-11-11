-- | This module contains the data types
-- | which represent the state of the game
module Model where

import Graphics.Gloss
import Data.List

-- | The Gamestate Model
data GameState = GameState {
  pacManStages:: [Picture],
  playState   :: PlayState,
  player      :: Player,
  level       :: Level,
  score       :: Score,
  highScores  :: [Int],
  ghostBitMaps:: [Picture],
  ghosts      :: [Ghost],
  frames      :: Float,
  timer       :: Float
  }

data PlayState  = StartNew | Start | Playing | Paused | GameOver | Win
type Score      = Int

-- | The Player Model
data Player = Player {
  playerLocation      :: Location,
  playerDirection     :: Direction,
  playerNextDirection :: NextDirection }

type Location       = (Float, Float)
data Direction      = North | South | West | East deriving Eq
type NextDirection  = Direction

-- | The Ghost Model
data Ghost = None | Ghost {
  ghostLocation       :: Location,
  ghostDirection      :: Direction,
  ghostNextDirection  :: NextDirection,
  ghostColor          :: GhostColor,
  ghostEatable        :: Eatable,
  ghostOutsideCage    :: GhostOutsideCage}

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

-- | The level model
data Level = Level {
  levelColor  :: Color,
  maze        :: Maze,
  ghostCage   :: Cage,
  food        :: Food,
  largeFood   :: LargeFood,
  playerSpawn :: PlayerSpawn,
  numGhosts   :: Int,
  ghostsSpawn :: [GhostSpawn]
}

type Maze       = [Location]
type Cage       = [Location]
type Food       = [Location]
type LargeFood  = [Location]
type PlayerSpawn = Location
type GhostSpawn = Location