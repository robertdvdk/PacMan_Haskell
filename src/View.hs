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
viewPure gstate = pictures ((viewMaze (maze (level gstate))) ++ ((viewCage (ghostCage (level gstate)) (frames gstate)) ++ ((viewFood (food (level gstate)))) ++ ((viewLargeFood (largeFood (level gstate)))) ++
  [ viewPlayer (player gstate),
    viewGhost (ghost1 gstate), 
    viewScore (score gstate), 
    viewState (playState gstate)] ++ 
    viewHighScores (highScores gstate)))

-- View individual components
viewMaze :: Maze -> [Picture]
viewMaze maze = [translate (x * 10) (y * 10) (color blue (rectangleSolid 10 10)) | (x, y) <- maze]

viewCage :: Cage -> Int -> [Picture]
viewCage cage f | f < 5 = [translate (x * 10) (y * 10) (color blue (rectangleSolid 10 10)) | (x, y) <- cage]
                | otherwise = [translate (x * 10) (y * 10) (color yellow (rectangleSolid 10 10)) | (x, y) <- cage]

viewPlayer :: Player -> Picture
viewPlayer player = translate (x * 10) (y * 10) (color yellow (circleSolid 5))
  where (x, y) = playerLocation player

viewFood :: Food -> [Picture]
viewFood food = [translate (x * 10) (y * 10) (color white (circleSolid 1 )) | (x, y) <- food]

viewLargeFood :: LargeFood -> [Picture]
viewLargeFood largefood = [translate (x * 10) (y * 10) (color white (circleSolid 4 )) | (x, y) <- largefood]

viewGhost :: Ghost -> Picture
viewGhost ghost = translate (x * 10) (y * 10) (color red (circleSolid 5))
  where (x, y) = ghostLocation ghost

viewScore :: Score -> Picture
viewScore score = translate (-250) (-240) (color white (scale 0.1 0.1 (text ("Score:" ++ (show score)))))

viewState :: PlayState -> Picture
viewState playState = translate (-250) 230 (color white (scale 0.1 0.1 (text playStateText)))
  where playStateText = case playState of 
          Begin     -> "Press 'p' to begin!"
          Paused    -> "Paused"
          GameOver  -> "Game Over! Press 'p' to go to start."
          _         -> ""

viewHighScores :: [Int] -> [Picture]
viewHighScores highScores = viewHighScoresSorted (sort highScores)

viewHighScoresSorted :: [Int] -> [Picture]
viewHighScoresSorted [s,t,u,v,w] = [translate 130 230 (color white (scale 0.1 0.1 (text "Highscores: ")))] ++
  [showScore 230 (show w), showScore 210 (show v), 
  showScore 190 (show u), showScore 170 (show t), showScore 150 (show s)]
  where showScore y z = translate 210 y (color white (scale 0.1 0.1 (text z)))