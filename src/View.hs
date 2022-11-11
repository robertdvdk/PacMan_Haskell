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
  viewPlayState   (playState gstate)              ++
  viewPlayer      gstate          (timer gstate)  ++
  viewLevel       (level gstate)  (frames gstate) ++
  viewScore       (score gstate)                  ++
  viewHighScores  (highScores gstate)             ++
  viewGhost       gstate (ghosts gstate)
  )

-- | View Level
viewLevel :: Level -> Float -> [Picture]
viewLevel level frames = 
  viewMaze (maze level) (levelColor level)  ++
  viewCage (ghostCage level) frames         ++
  viewFood (food level)                     ++
  viewLargeFood (largeFood level)

-- | View individual components
viewMaze :: Maze -> Color -> [Picture]
viewMaze maze levelColor = [translate (x * 10) (y * 10) (color levelColor (rectangleSolid 10 10)) | (x, y) <- maze]

viewCage :: Cage -> Float -> [Picture]
viewCage cage f | f < 5     = [translate (x * 10) (y * 10) (color blue    (rectangleSolid 10 10)) | (x, y) <- cage]
                | otherwise = [translate (x * 10) (y * 10) (color yellow  (rectangleSolid 10 10)) | (x, y) <- cage]

viewPlayer :: GameState -> Float -> [Picture]
viewPlayer gstate t = case playState gstate of
  GameOver  -> viewPlayerDying  gstate t
  _         -> viewPlayerAlive gstate 

viewPlayerDying :: GameState -> Float -> [Picture]
viewPlayerDying gstate t
  |             t < 4   = showPacManStage stage1
  | 4   <= t && t < 7   = showPacManStage stage2
  | 7   <= t && t < 10  = showPacManStage stage3
  | 10  <= t && t < 13  = showPacManStage stage4
  | otherwise           = showPacManStage stage5
  where [stage1, stage2, stage3, stage4, stage5] = pacManStages gstate
        showPacManStage stage = [translate (x * 10) (y * 10) (scale 0.03 0.03 stage)]
            where (x, y) = playerLocation (player gstate)
                

viewPlayerAlive :: GameState -> [Picture]
viewPlayerAlive gstate = let rotatePacMan angle (stage1:_)= [translate (x * 10) (y * 10) (scale 0.03 0.03 (rotate angle stage1))] 
  in case (playerDirection (player gstate)) of
  East  -> rotatePacMan 0   (pacManStages gstate)
  South -> rotatePacMan 90  (pacManStages gstate)
  West  -> rotatePacMan 180 (pacManStages gstate)
  North -> rotatePacMan 270 (pacManStages gstate)
  where (x, y) = playerLocation (player gstate)

viewFood :: Food -> [Picture]
viewFood food = [translate (x * 10) (y * 10) (color white (circleSolid 1 )) | (x, y) <- food]

viewLargeFood :: LargeFood -> [Picture]
viewLargeFood largefood = [translate (x * 10) (y * 10) (color white (circleSolid 4)) | (x, y) <- largefood]

viewGhost :: GameState -> [Ghost] -> [Picture]
-- viewGhost gstate = [translate (x * 10) (y * 10) (scale 0.045 0.045 (color red redGhost))] -- ff veranderd omdat ik de bitmap niet heb
viewGhost gstate [] = []
viewGhost gstate (g:gs) = case ghostColor g of
  Red -> [translate (x * 10) (y * 10) (scale 1 1 (color red (circleSolid 5)))] ++ viewGhost gstate gs
  Blue -> [translate (x * 10) (y * 10) (scale 1 1 (color blue (circleSolid 5)))] ++ viewGhost gstate gs
  Yellow -> [translate (x * 10) (y * 10) (scale 1 1 (color yellow (circleSolid 5)))] ++ viewGhost gstate gs
  Pink -> [translate (x * 10) (y * 10) (scale 1 1 (color magenta (circleSolid 5)))] ++ viewGhost gstate gs
  where (x, y) = ghostLocation g
        [redGhost, _, _, _] = ghostBitMaps gstate

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