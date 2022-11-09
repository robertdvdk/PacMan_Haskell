-- | This module contains the level design
-- | and initial state
module Levels where

import Model
import Graphics.Gloss
import Data.List

-- | Level and Initial State
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
level1 = Level blue maze1 cage1 food1 largefood1

level2 :: Level
level2 = Level red maze1 cage1 food1 largefood1

level3 :: Level
level3 = Level green maze1 cage1 food1 largefood1

initialGhost1 :: Ghost
initialGhost1 = Ghost (0, 15) West East Red NotEatable InsideCage

initialPlayer :: Player
initialPlayer = Player (0, 0) West West

initialState :: GameState
initialState = GameState Start initialPlayer level1 10 [0, 0, 0, 0, 0] initialGhost1 0