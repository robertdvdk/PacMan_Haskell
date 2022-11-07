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
viewPure gstate = pictures ( 
  viewPlayState   (playState gstate)  ++
  viewPlayer      (player gstate)     ++
  viewLevel       (level gstate) (frames gstate) ++
  viewScore       (score gstate)      ++
  viewHighScores  (highScores gstate) ++
  viewGhost       (ghost1 gstate))
  
-- | View Level
viewLevel :: Level -> Int -> [Picture]
viewLevel level frames = 
  viewMaze (maze level) (levelColor level)  ++
  viewCage (ghostCage level) frames         ++
  viewFood (food level)                     ++
  viewLargeFood (largeFood level)

-- | View individual components
viewMaze :: Maze -> Color -> [Picture]
viewMaze maze levelColor = [translate (x * 10) (y * 10) (color levelColor (rectangleSolid 10 10)) | (x, y) <- maze]

viewCage :: Cage -> Int -> [Picture]
viewCage cage f | f < 5     = [translate (x * 10) (y * 10) (color blue    (rectangleSolid 10 10)) | (x, y) <- cage]
                | otherwise = [translate (x * 10) (y * 10) (color yellow  (rectangleSolid 10 10)) | (x, y) <- cage]

viewPlayer :: Player -> [Picture]
viewPlayer player = [translate (x * 10) (y * 10) (color yellow (circleSolid 5))]
  where (x, y) = playerLocation player

viewFood :: Food -> [Picture]
viewFood food = [translate (x * 10) (y * 10) (color white (circleSolid 1 )) | (x, y) <- food]

viewLargeFood :: LargeFood -> [Picture]
viewLargeFood largefood = [translate (x * 10) (y * 10) (color white (circleSolid 4 )) | (x, y) <- largefood]

viewGhost :: Ghost -> [Picture]
viewGhost ghost = [translate (x * 10) (y * 10) (color red (circleSolid 5))]
  where (x, y) = ghostLocation ghost

viewScore :: Score -> [Picture]
viewScore score = [translate (-250) (-240) (color white (scale 0.1 0.1 (text ("Score:" ++ (show score)))))]

viewPlayState :: PlayState -> [Picture]
viewPlayState playState = [translate (-250) 230 (color white (scale 0.1 0.1 (text playStateText)))]
  where playStateText = case playState of 
          Start     -> "Press 'p' to start!"
          Paused    -> "Paused. Press 'p' to resume."
          GameOver  -> "Game Over! Press 'p' to go to start."
          Win       -> "Congratulations! Press 'p' to go to the next level."
          _         -> "Press 'p' to pause"

viewHighScores :: [Int] -> [Picture]
viewHighScores highScores = viewHighScoresSorted (sort highScores)

viewHighScoresSorted :: [Int] -> [Picture]
viewHighScoresSorted [s,t,u,v,w] = [translate 130 230 (color white (scale 0.1 0.1 (text "Highscores: ")))] ++
  [showScore 230 (show w), showScore 210 (show v), 
  showScore 190 (show u), showScore 170 (show t), showScore 150 (show s)]
  where showScore y z = translate 210 y (color white (scale 0.1 0.1 (text z)))