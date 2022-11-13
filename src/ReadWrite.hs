-- | This module defines how the data 
--   can be read and written
module ReadWrite where

import Model
import Data.List
import System.Directory

filePath = "ScorePacMan.txt"


createFile = do fileExists <- doesFileExist filePath
                if fileExists then return () else writeFile filePath "0\n0\n0\n0\n0"

-- | Read the file of highscores.
readF :: IO [Int]
readF = do
            content <- readFile filePath
            return (map read (lines content))

-- | Updates the list of high scores in the game
updateHighScore :: GameState -> [Int]
updateHighScore gstate  | score gstate > minimum scores = changeElement (score gstate) (minimum scores) scores
                        | otherwise = scores
    where   scores = highScores gstate
            changeElement score el [] = []
            changeElement score el (x:xs)   | el == x   = score : xs
                                            | otherwise = x     : (changeElement score el xs)

-- | Writes the list of high scores in the game to the file.
writeF :: GameState -> IO ()
writeF gstate = writeFile filePath (unlines (map show (sort (updateHighScore gstate))))

