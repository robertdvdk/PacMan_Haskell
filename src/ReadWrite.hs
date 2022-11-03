-- | This module defines how the data 
--   can be read and written
module ReadWrite where

import Model

write :: Score -> IO ()
write score = do 
    writeFile "ScorePacMan.txt" (show score)