-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

-- View all components
view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture 
viewPure gstate = pictures (viewScore (score gstate) : (viewPlayer (player gstate) : (viewLevel (level gstate))))

-- View individual components
viewScore :: Score -> Picture
viewScore score = translate (150) (150) (color green (text (show score)))

viewPlayer :: Player -> Picture
viewPlayer player = translate (x * 10) (y * 10) (color yellow (circleSolid 5))
  where (x, y) = playerLocation player

viewLevel :: Level -> [Picture]
viewLevel level = [translate (x * 10) (y * 10) (color blue (rectangleSolid 10 10)) | (x, y) <- level]