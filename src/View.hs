-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

-- View all components
view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture 
viewPure gstate = pictures ((viewLevel (level gstate)) ++ 
  [ viewPlayer (player gstate), 
    viewScore (score gstate), 
    viewState (playState gstate)] ++ 
    viewHighScores (highScores gstate))

-- View individual components
viewLevel :: Level -> [Picture]
viewLevel level = [translate (x * 10) (y * 10) (color blue (rectangleSolid 10 10)) | (x, y) <- level]

viewPlayer :: Player -> Picture
viewPlayer player = translate (x * 10) (y * 10) (color yellow (circleSolid 5))
  where (x, y) = playerLocation player

viewScore :: Score -> Picture
viewScore score = translate (-200) (-190) (color white (scale 0.1 0.1 (text ("Score:" ++ (show score)))))

viewState :: PlayState -> Picture
viewState playState = translate (-200) 180 (color white (scale 0.1 0.1 (text playStateText)))
  where playStateText = case playState of 
          Begin     -> "Press 'p' to begin!"
          Paused    -> "Paused"
          GameOver  -> "Game Over! Press 'p' to go to start."
          _         -> ""

viewHighScores :: [Int] -> [Picture]
viewHighScores [s,t,u,v,w] = [translate 80 180 (color white (scale 0.1 0.1 (text "Highscores: ")))] ++
  [laatzien 180 (show s), laatzien 160 (show t), 
  laatzien 140 (show u), laatzien 120 (show v), laatzien 100 (show w)]
  where laatzien y z = translate 160 y (color white (scale 0.1 0.1 (text z)))