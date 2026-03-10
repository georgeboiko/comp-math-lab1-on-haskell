module Utils.EquationStorage (Equation(..), getEquationById) where

data Equation = Eq
    { equation  :: Double -> Double
    , equation' :: Double -> Double
    , equation'' :: Double -> Double
    , fString    :: String
    , fLatex     :: String
    }

equations :: [Equation]
equations =
    [ Eq (\x -> x**2 + 4*x + 2) (\x -> 2*x + 4) (const 2) "x^2 + 4*x + 2" "x^2 + 4x + 2"
    , Eq (\x -> x**2 - 4) (2 *) (const 2) "x^2 - 4" "x^2 - 4"
    , Eq (\x -> sin (2*x) + 5) (\x -> 2 * cos (2*x)) (\x -> -(4 * sin (2 * x))) "sin(2*x) + 5" "\\sin(2x) + 5"
    ]

getEquationById :: Int -> Equation
getEquationById index = equations !! index