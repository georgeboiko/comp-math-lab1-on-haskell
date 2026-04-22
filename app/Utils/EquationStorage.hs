module Utils.EquationStorage (Equation(..), SystemEquation(..), FunctionEq(..), Distribution(..), getDistFunction,
    getEquationById, getSystemById, getFunctionById,
    equations, systems, functions) where

import Types.MathTypes

data Equation = Eq
    { equation :: Double -> Double
    , equation' :: Double -> Double
    , equation'' :: Double -> Double
    , fString :: String
    , fLatex :: String
    }

data SystemEquation = SystemEq
    { systemFs :: [Vector -> Double]
    , systemJacobian :: Vector -> Matrix
    , systemStrings :: [String]
    , systemLatex :: String
    }

data Distribution = UniformDist
    | NormalDist { mean :: Double, stdDev :: Double }
    | ExponentialDist { lambda :: Double }

getDistFunction :: Distribution -> Double -> Double -> (Double -> Double)
getDistFunction UniformDist a b =
    \_ -> 1 / (b - a)
getDistFunction (ExponentialDist l) _ _ =
    \x -> l * exp (- (l * x))
getDistFunction (NormalDist m s) _ _ =
    \x -> (1 / (s * sqrt (2 * pi))) * exp (- (0.5 * ((x - m) / s) ** 2))

data FunctionEq = FunctionEq
    { functionEq :: Double -> Double
    , distribution :: Distribution
    , badPoint :: Maybe Double
    , functionString :: String
    , functionLatex :: String
    }

equations :: [Equation]
equations =
    [ Eq (\x -> x**2 + 4*x + 2) (\x -> 2*x + 4) (const 2) "x^2 + 4*x + 2" "x^2 + 4x + 2"
    , Eq (\x -> x**2 - 4) (2 *) (const 2) "x^2 - 4" "x^2 - 4"
    , Eq (\x -> sin (2*x) + 5) (\x -> 2 * cos (2*x)) (\x -> -(4 * sin (2 * x))) "sin(2*x) + 5" "\\sin(2x) + 5"
    ]

functions :: [FunctionEq]
functions =
    [ FunctionEq (\x -> x**2 + 4*x + 2) UniformDist Nothing "x^2 + 4*x + 2" "x^2 + 4x + 2"
    , FunctionEq (\x -> x**2 - 4) UniformDist Nothing "x^2 - 4" "x^2 - 4"
    , FunctionEq (\x -> sin (2*x) + 5) UniformDist Nothing "sin(2*x) + 5" "\\sin(2x) + 5"
    , FunctionEq (1 /) (ExponentialDist 1.5) (Just 0.0) "1/x" "\\frac{1}{x}"
    , FunctionEq (\x -> 1 / sqrt x) (ExponentialDist 1.5) (Just 0.0) "1/sqrt(x)" "\\frac{1}{\\sqrt{x}}"
    , FunctionEq (\x -> sin x / x) (NormalDist 0 1) (Just 0.0) "sin(x)/x" "\\frac{\\sin{x}}{x}"
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
getEquationById index
    | index >= 0 = equations !! index
    | otherwise = head equations

getSystemById :: Int -> SystemEquation
getSystemById index
    | index >= 0 = systems !! index
    | otherwise = head systems

getFunctionById :: Int -> FunctionEq
getFunctionById index
    | index >= 0 = functions !! index
    | otherwise = head functions
