module Utils.EquationStorage (Equation(..), getEquationById) where

data Equation = Eq 
    { equation  :: Double -> Double
    , equation' :: Double -> Double
    }

equations :: [Equation]
equations =
    [ Eq (\x -> x**2 + 4*x + 2) (\x -> 2*x + 4)
    , Eq (\x -> x**2 - 4)     (\x -> 2*x)
    , Eq (\x -> sin (2*x) + 5) (\x -> 2 * cos (2*x))
    ]

getEquationById :: Int -> Equation
getEquationById index = equations !! index