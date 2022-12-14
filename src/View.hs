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
  viewPlayer       gstate                         ++
  viewLevel       (level gstate)                  ++
  viewScore       (score gstate)                  ++
  viewHighScores  (highScores gstate)             ++
  viewGhosts      (ghosts (level gstate)) gstate  ++
  viewLives       (playerLives (player gstate))   ++
  viewEatable     (ghostsEatable (level gstate))
  )

-- | View Pac-Man 
viewPlayer :: GameState -> [Picture]
viewPlayer gstate = case playState gstate of
  GameOver  -> viewPlayerDying (player gstate)
  _         -> viewPlayerAlive (player gstate)

viewPlayerDying :: Player -> [Picture]
viewPlayerDying player
  |             t < 4   = showPacManStage stage1
  | 4   <= t && t < 7   = showPacManStage stage2
  | 7   <= t && t < 10  = showPacManStage stage3
  | 10  <= t && t < 13  = showPacManStage stage4
  | otherwise           = showPacManStage stage5
  where t = dyingTimer player
        [stage1, stage2, stage3, stage4, stage5] = pacManBitMaps player
        showPacManStage stage = [translate (x * 10) (y * 10) (scale 0.03 0.03 stage)]
            where (x, y) = playerLocation player
                
viewPlayerAlive :: Player -> [Picture]
viewPlayerAlive player = let rotatePacMan angle (stage1:_) = [translate (x * 10) (y * 10) (scale 0.03 0.03 (rotate angle stage1))] 
  in case (playerDirection player) of
  East  -> rotatePacMan 0   (pacManBitMaps player)
  South -> rotatePacMan 90  (pacManBitMaps player)
  West  -> rotatePacMan 180 (pacManBitMaps player)
  North -> rotatePacMan 270 (pacManBitMaps player)
  where (x, y) = playerLocation player

-- | View Level
viewLevel :: Level -> [Picture]
viewLevel level = 
  viewMaze level                  ++
  viewCage level                  ++
  viewFood (food level)           ++
  viewLargeFood (largeFood level)

-- | View individual level components
viewMaze :: Level -> [Picture]
viewMaze level = [translate (x * 10) (y * 10) (Color (levelColor level) (rectangleSolid 10 10)) | (x, y) <- maze level]

viewCage :: Level -> [Picture]
viewCage level  
  | cageTimer level < 5 = [translate (x * 10) (y * 10) (color (levelColor level)  (rectangleSolid 10 10)) | (x, y) <- ghostCage level]
  | otherwise           = [translate (x * 10) (y * 10) (color yellow              (rectangleSolid 10 10)) | (x, y) <- ghostCage level]

viewFood :: Food -> [Picture]
viewFood food = [translate (x * 10) (y * 10) (color white (circleSolid 1 )) | (x, y) <- food]

viewLargeFood :: LargeFood -> [Picture]
viewLargeFood largefood = [translate (x * 10) (y * 10) (color white (circleSolid 4)) | (x, y) <- largefood]

-- | View Ghosts
viewGhosts :: [Ghost] -> GameState -> [Picture]
viewGhosts [] _ = []
viewGhosts (ghost:gs) gstate = case ghostsEatable (level gstate) of 
  (NotEatable, _) -> [translate (x * 10) (y * 10) (scale 0.15 0.15 (ghostBitMap ghost))] ++ viewGhosts gs gstate
  (Eatable, _) -> [translate (x * 10) (y * 10) (scale 0.15 0.15 (eatableGhostBitMap gstate))] ++ viewGhosts gs gstate
  where (x, y) = ghostLocation ghost

-- | View Text
viewScore :: Score -> [Picture]
viewScore score = [translate (-275) (-265) (color white (scale 0.1 0.1 (text ("Score:" ++ (show score)))))]

viewPlayState :: PlayState -> [Picture]
viewPlayState playState = [translate (-275) 255 (color white (scale 0.1 0.1 (text playStateText)))]
  where playStateText = case playState of 
          Start     -> "Press 'p' to start!"
          Paused    -> "Paused. Press 'p' to resume."
          GameOver  -> "Game Over! Press 'p' to go to start."
          Win       -> "Congratulations! Press 'p' to go to the next level."
          WonEntireGame -> "Well done! You beat the entire game. Press p to play again."
          _         -> "Press 'p' to pause"

viewHighScores :: [Int] -> [Picture]
viewHighScores highScores = viewHighScoresSorted (sort highScores)

viewHighScoresSorted :: [Int] -> [Picture]
viewHighScoresSorted [s,t,u,v,w] = [translate 155 255 (color white (scale 0.1 0.1 (text "Highscores: ")))] ++
  [showScore 255 (show w), showScore 235 (show v), 
  showScore 215 (show u), showScore 195 (show t), showScore 175 (show s)]
  where showScore y z = translate 235 y (color white (scale 0.1 0.1 (text z)))

viewLives :: Int -> [Picture]
viewLives n = [translate (220) (-265) (color white (scale 0.1 0.1 (text ("Lives:" ++ (show n)))))]

viewEatable :: (IsEatable, Float) -> [Picture]
viewEatable (a, n) = [translate (0) (-265) (color white (scale 0.1 0.1 (text (show a))))]