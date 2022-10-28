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



getLocation :: Player -> (Float, Float)
getLocation (Player a _ _ _) = a

drawLevel :: [Point] -> Player -> Picture
drawLevel level player
  = pictures (translate p1 p2 (color yellow (Circle 10)) : [translate (fst point) (snd point) (color blue (rectangleSolid 10 10)) | point <- level]) where (p1, p2) = playerLocation player