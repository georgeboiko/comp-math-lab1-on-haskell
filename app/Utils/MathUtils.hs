module Utils.MathUtils (hasRoot, isUniqueRoot) where

hasRoot :: (Double -> Double) -> Double -> Double -> Bool
hasRoot f a b = f a * f b < 0

isUniqueRoot :: (Double -> Double) -> (Double -> Double) -> Double -> Double -> Double -> Bool
isUniqueRoot f f' a b step =
    hasRoot f a b && not (any (> 0) derivs && any (< 0) derivs)
    where
        derivs = map f' [a, a + step .. b]