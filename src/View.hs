-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Data.List

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
viewHighScores highScores = viewHighScoresSorted (sort highScores)

viewHighScoresSorted :: [Int] -> [Picture]
viewHighScoresSorted [s,t,u,v,w] = [translate 80 180 (color white (scale 0.1 0.1 (text "Highscores: ")))] ++
  [showScore 180 (show w), showScore 160 (show v), 
  showScore 140 (show u), showScore 120 (show t), showScore 100 (show s)]
  where showScore y z = translate 160 y (color white (scale 0.1 0.1 (text z)))