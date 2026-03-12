module Utils.EquationStorage (Equation(..), SystemEquation(..), getEquationById, getSystemById, equations, systems) where

import Types.MathTypes

data Equation = Eq
    { equation  :: Double -> Double
    , equation' :: Double -> Double
    , equation'' :: Double -> Double
    , fString    :: String
    , fLatex     :: String
    }

data SystemEquation = SystemEq
    { systemFs      :: [Vector -> Double]
    , systemJacobian :: Vector -> Matrix
    , systemStrings :: [String]
    , systemLatex   :: String
    }

equations :: [Equation]
equations =
    [ Eq (\x -> x**2 + 4*x + 2) (\x -> 2*x + 4) (const 2) "x^2 + 4*x + 2" "x^2 + 4x + 2"
    , Eq (\x -> x**2 - 4) (2 *) (const 2) "x^2 - 4" "x^2 - 4"
    , Eq (\x -> sin (2*x) + 5) (\x -> 2 * cos (2*x)) (\x -> -(4 * sin (2 * x))) "sin(2*x) + 5" "\\sin(2x) + 5"
    ]

systems :: [SystemEquation]
systems =
    [
      SystemEq
        { systemFs = [ \[x, y] -> x**2 + y**2 - 4
                     , \[x, y] -> y - 3*x**2 ]
        , systemJacobian = \[x, y] -> [ [2*x, 2*y]
                                     , [-6*x, 1] ]
        , systemStrings = ["x^2 + y^2 - 4", "y - 3*x^2"]
        , systemLatex = "\\begin{cases} x^2 + y^2 = 4 \\\\ y = 3x^2 \\end{cases}"
        }
    ,
      SystemEq
        { systemFs = [ \[x, y] -> sin x - y + 1
                     , \[x, y] -> y + cos x - 2 ]
        , systemJacobian = \[x, y] -> [ [cos x, -1]
                                     , [-sin x, 1] ]
        , systemStrings = ["sin(x) - y + 1", "y + cos(x) - 2"]
        , systemLatex = "\\begin{cases} \\sin(x) = y - 1 \\\\ y + \\cos(x) = 2 \\end{cases}"
        }
    ]

getEquationById :: Int -> Equation
getEquationById index = equations !! index

getSystemById :: Int -> SystemEquation
getSystemById index = systems !! index