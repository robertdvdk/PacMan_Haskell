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
                    where
                      (p1,p2) = getLocation p

getLocation :: Player -> (Float, Float)
getLocation (Player a _ _ _) = a