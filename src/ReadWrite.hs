-- | This module defines how the data 
--   can be read and written
module ReadWrite where

import Model
import Data.List

readF :: IO [Int]
readF = do  --make highscores a list of strings and give that to high scores then alter view
            content <- readFile "/Users/robertvdklis/Documents/code/Courses/functioneel-programmeren/project/ScorePacMan.txt"
            return (map read (lines content))

updateHighScore :: GameState -> [Int]
updateHighScore gstate  | score gstate > minimum scores = changeElement (score gstate) (minimum scores) scores
                        | otherwise = scores
    where   scores = highScores gstate
            changeElement score el [] = []
            changeElement score el (x:xs)   | el == x   = score : (changeElement score el xs)
                                            | otherwise = x     : xs

writeF :: GameState -> IO ()
writeF gstate = writeFile filePath (unlines (map show (sort (updateHighScore gstate))))
                        where filePath = "/Users/robertvdklis/Documents/code/Courses/functioneel-programmeren/project/ScorePacMan.txt"

