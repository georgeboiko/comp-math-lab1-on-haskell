module Utils.Generators (generateMatrix, generateVector, generateRandomValues) where

import System.Random (randomRIO)
import Types.MathTypes

generateVector :: Int -> IO Vector
generateVector 0 = return []
generateVector size = do
    val <- randomRIO(1.0, 100.0)
    other <- generateVector $ size-1
    return $ val : other

generateRandomValues :: Int -> Double -> Double -> IO Vector
generateRandomValues 0 _ _ = return []
generateRandomValues size a b = do
    val <- randomRIO(a, b)
    other <- generateRandomValues (size - 1) a b
    return $ val : other

generateMatrix :: Int -> Int -> IO Matrix
generateMatrix 0 _ = return []
generateMatrix rows size = do
    val <- generateVector size
    other <- generateMatrix (rows - 1) size
    return $ val : other