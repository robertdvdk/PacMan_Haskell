-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowNothing   -> blank
  ShowANumber n -> color blue (text (show n))
  ShowAChar   c -> color green (text [c])
  ShowAPlayer p -> translate p1 p2 (color yellow (Circle 10))
                    where (p1, p2) = playerLocation p
  ShowGame l p -> drawLevel l p
  -- ShowGame l p -> drawLevel l



getLocation :: Player -> (Float, Float)
getLocation (Player a _ _ _) = a

-- drawLevel :: [Point] -> Player -> Picture
-- drawLevel level player
  -- = pictures (translate p1*20 p2*20 (color yellow (Circle 20)) : [translate (fst point * 20) (snd point * 20) (color blue (rectangleSolid 10 10)) | point <- level]) where (p1, p2) = playerLocation player
drawLevel :: [Point] -> Player -> Picture
-- drawLevel level player = pictures (translate p1*20 p2*20 (color yellow (Circle 20)) : [translate (fst point * 20) (snd point * 20) (color blue (rectangleSolid 20 20)) | point <- level]) where (p1, p2) = playerLocation player
drawLevel level player = pictures (translate (p1 * 10) (p2 * 10) (color yellow (Circle 5)) : ([translate (fst point * 10) (snd point * 10) (color blue (rectangleSolid 10 10)) | point <- level])) where (p1, p2) = playerLocation player