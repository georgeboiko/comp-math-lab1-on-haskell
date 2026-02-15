module Generators (generateMatrix, generateVector) where

import Types
import System.Random (randomRIO)

generateVector :: Int -> IO Vector
generateVector 0 = return []
generateVector size = do
    val <- randomRIO(1.0, 100.0)
    other <- generateVector $ size-1
    return $ val : other

generateMatrix :: Int -> Int -> IO Matrix
generateMatrix 0 _ = return []
generateMatrix rows size = do
    val <- generateVector size
    other <- generateMatrix (rows - 1) size
    return $ val : other