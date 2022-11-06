-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss
import Data.List

-- The Gamestate Model
data GameState = GameState {
  playState   :: PlayState,
  player      :: Player,
  level       :: Level,
  score       :: Score,
  highScores  :: [Int],
  ghost1      :: Ghost,
  frames      :: Int}

data PlayState  = Begin | Playing | Paused | GameOver
type Score      = Int

-- The Player Model
data Player = Player {
  playerLocation      :: Location,
  playerDirection     :: Direction,
  playerNextDirection :: NextDirection }

type Location       = (Float, Float)
data Direction      = North | South | West | East deriving Eq
type NextDirection  = Direction

-- The Ghost Model
data Ghost = Ghost {
  ghostLocation       :: Location,
  ghostDirection      :: Direction,
  ghostNextDirection  :: NextDirection,
  ghostColor          :: GhostColor,
  ghostEatable        :: Eatable,
  ghostOutsideCage    :: GhostOutsideCage}

data GhostOutsideCage = InsideCage | OutsideCage
data GhostColor    = Red | Pink | Yellow | Blue
data Eatable       = IsEatable | NotEatable

class Entity a where
  entityLocation :: a -> Location
  entityDirection :: a -> Direction

instance Entity Ghost where
  entityLocation = ghostLocation
  entityDirection = ghostDirection

instance Entity Player where
  entityLocation = playerLocation
  entityDirection = playerDirection

data Level = Level {
  maze :: Maze,
  ghostCage :: Cage,
  food :: Food,
  largeFood :: LargeFood
}
type Maze = [Location]
type Cage = [Location]
type Food = [Location]
type LargeFood = [Location]

-- Level and Initial State
makeLevelRectangle :: (Location, Location) -> [Location]
makeLevelRectangle ((x1, y1), (x2, y2)) = [(x, y) | x <- [x1..x2], y <- [y1, y2]] ++ [(x, y) | x <- [x1, x2], y <- [y1..y2]]

maze1 = concatMap makeLevelRectangle 
  [((-18, -18),  (18, 18)), 
  ((-6, 13),     (6, 18)),
  ((-16, -1),     (-8, 16)),
  ((-6, -1),      (6, -1)), 
  ((8, 8),      (16, 16)), 
  ((-6, 1),      (6, 11)), 
  ((-16, -6),   (6, -3)), 
  ((8, -6),     (16, 6)), 
  ((-16, -10),   (16, -8)), 
  ((-16, -12),  (0, -12)), 
  ((2, -12),    (16, -12)), 
  ((-16, -16),  (-2, -14)), 
  ((0, -16),     (16, -14))]

cage1 = makeLevelRectangle ((-6, 14), (-6, 15))

largefood1 = nub $ concatMap makeLevelRectangle [((-17, -17), (-17, -17)), ((-17, 17), (-17, 17)), ((17, -17), (17, -17)), ((17, 17), (17, 17))]
-- | Remove duplicates: using nub, duplicates with in the list of small dots itself are removed.
-- | With foldr, the small dots in corners that contain both a large dot and a small dot are removed.
food1 = foldr delete (nub $ concatMap makeLevelRectangle
  [((-17, -17),  (-17, 17)),
   ((-17, 17),  (-7, 17)),
   ((7, 17),    (17, 17)),
   ((-17, -17),   (17, -17)),
   ((17, -17),    (17, 17)),
   ((-7, 0),    (7, 0)),
   ((-7, -2),   (-7, 16)),
   ((7, -7),    (7, 16)),
   ((-16, -2),  (7, -2)),
   ((7, 7),     (16, 7)),
   ((-16, -7),  (16, -7)),
   ((-16, -11), (16, -11)),
   ((-16, -13), (16, -13)),
   ((1, -13),   (1, -11)),
   ((-1, -16),  (-1, -14))])
   largefood1

level1 :: Level
level1 = Level maze1 cage1 food1 largefood1

firstGhost :: Ghost
firstGhost = Ghost (0, 15) West East Red NotEatable InsideCage

initialPlayer :: Player
initialPlayer = Player (0, 0) West West

initialState :: GameState
initialState = GameState Begin initialPlayer level1 10 [0, 0, 0, 0, 0] firstGhost 0