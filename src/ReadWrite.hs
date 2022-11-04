-- | This module defines how the data 
--   can be read and written
module ReadWrite where

import Model
import Data.List
import Data.Sequence

readF :: IO [Int]
readF = do  --make highscores a list of strings and give that to high scores then alter view
            content <- readFile "C:/Users/Jurre Luijten/Documents/Master/Functioneel Programmeren/Game/High Scores.txt"
            return (map read (lines content))

updateHighScore :: GameState -> [Int]
updateHighScore gstate  | score gstate > minimum (highScores gstate) = changeElement (score gstate) (minimum (highScores gstate)) (highScores gstate)
                        | otherwise = highScores gstate

changeElement score el [] = []
changeElement score el (x:xs)   | el == x   = score : (changeElement score el xs)
                                | otherwise = el    : (changeElement score el xs)

writeF :: GameState -> IO ()
writeF gstate = writeFile filePath (unlines (map show (updateHighScore gstate)))
                        where filePath = "C:/Users/Jurre Luijten/Documents/Master/Functioneel Programmeren/Game/High Scores.txt"

