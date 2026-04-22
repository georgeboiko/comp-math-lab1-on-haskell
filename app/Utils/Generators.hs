module Utils.Generators (generateMatrix, generateVector, generateRandomValues, generateDistributionValues) where

import Types.MathTypes
import Utils.EquationStorage
import System.Random.Stateful
import Utils.MathUtils (transformToDist)

getRawStream :: (Double, Double) -> IO [Double]
getRawStream (low, high) = do
    gen <- newStdGen
    return $ randomRs (low, high) gen

generateVector :: Int -> IO Vector
generateVector size = do
    stream <- getRawStream (1.0, 100.0)
    return $ take size stream

generateRandomValues :: Int -> Double -> Double -> IO Vector
generateRandomValues size a b = do
    stream <- getRawStream (a, b)
    return $ take size stream

generateDistributionValues :: Distribution -> Int -> Double -> Double -> IO Vector
generateDistributionValues dist n a b = do
    rawRandoms <- getRawStream (0.0001, 0.9999)
    return $ take n $ filter (\x -> x >= a && x <= b) (transformToDist dist a b rawRandoms)

generateMatrix :: Int  -> IO Matrix
generateMatrix rows = do
    stream <- getRawStream (1.0, 100.0)
    return $ chunkList rows $ take (rows*rows) stream

chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs = take n xs : chunkList n (drop n xs)