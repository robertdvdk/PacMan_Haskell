-- | This module defines how the data 
--   can be read and written
module ReadWrite where

import Model
import Data.List

readF :: IO [Int]
readF = do  --make highscores a list of strings and give that to high scores then alter view
            content <- readFile "C:/Users/Jurre Luijten/Documents/Master/Functioneel Programmeren/Game/High Scores.txt"
            return (map read (lines content))

updateHighScore :: GameState -> [Int]
updateHighScore gstate  | score gstate > minimum scores = changeElement (score gstate) (minimum scores) scores
                        | otherwise = scores
    where   scores = highScores gstate
            changeElement score el [] = []
            changeElement score el (x:xs)   | el == x   = score : (changeElement score el xs)
                                            | otherwise = x     : (changeElement score el xs)

writeF :: GameState -> IO ()
writeF gstate = writeFile filePath (unlines (map show (sort (updateHighScore gstate))))
                        where filePath = "C:/Users/Jurre Luijten/Documents/Master/Functioneel Programmeren/Game/High Scores.txt"

