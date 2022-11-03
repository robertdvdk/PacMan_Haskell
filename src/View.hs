-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

-- View all components
view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture 
viewPure gstate = pictures ((viewLevel (level gstate)) ++ [viewPlayer (player gstate), viewScore (score gstate), viewState (playState gstate)])

-- View individual components
viewLevel :: Level -> [Picture]
viewLevel level = [translate (x * 10) (y * 10) (color blue (rectangleSolid 10 10)) | (x, y) <- level]

viewPlayer :: Player -> Picture
viewPlayer player = translate (x * 10) (y * 10) (color yellow (circleSolid 5))
  where (x, y) = playerLocation player

viewScore :: Score -> Picture
viewScore score = translate (-200) (-200) (color white (scale 0.2 0.2 (text ("Score:" ++ (show score)))))

viewState :: PlayState -> Picture
viewState playState = translate (-200) 180 (color white (scale 0.2 0.2 (text playStateText)))
  where playStateText = case playState of 
          Begin     -> "Press 'p' to begin!"
          Paused    -> "Paused"
          GameOver  -> "Game Over! Press 'p'to start again."
          _         -> ""