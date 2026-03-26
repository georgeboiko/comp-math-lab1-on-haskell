module Methods.Lab3RectanglesMethod (LeftRectanglesMethod(..), MiddleRectanglesMethod(..), RightRectanglesMethod(..)) where
import Types.SolverTypes
import Utils.EquationStorage

class RectanglesStrategy s where
    getPointOffset :: s -> Double

data LeftRectanglesMethod = LeftRectanglesMethod
data MiddleRectanglesMethod = MiddleRectanglesMethod
data RightRectanglesMethod = RightRectanglesMethod

instance RectanglesStrategy LeftRectanglesMethod where getPointOffset _ = 0
instance RectanglesStrategy MiddleRectanglesMethod where getPointOffset _ = 0.5
instance RectanglesStrategy RightRectanglesMethod where getPointOffset _ = 1

instance IntegralSolver LeftRectanglesMethod where solveIntegral = genericSolve
instance IntegralSolver MiddleRectanglesMethod where solveIntegral = genericSolve
instance IntegralSolver RightRectanglesMethod where solveIntegral = genericSolve

genericSolve :: (RectanglesStrategy s) => s -> FunctionEq -> Double -> Double -> Double -> SolverIntegralOutputData
genericSolve strategy func a b eps = solve strategy (functionEq func) a b eps 4 (calcRectangle strategy (functionEq func) a b 2)

calcRectangle :: (RectanglesStrategy s) => s -> (Double -> Double) -> Double -> Double -> Int -> Double
calcRectangle strategy f a b n = 
    h * sum values
    where
        h = (b - a) / fromIntegral n
        offset = getPointOffset strategy
        values = [f (a + (fromIntegral i + offset) * h) | i <- [0 .. n-1]]

solve :: (RectanglesStrategy s) => s -> (Double -> Double) -> Double -> Double -> Double -> Int -> Double -> SolverIntegralOutputData
solve strategy f a b eps n oldVal
    | runge < eps = SolverIntegralOutputData {
        isIntegralSuccessfully = True
        , calculatedIntegral = curVal
        , errIntegralVal = runge
        , partsCount = n
        , integralInfromationMsg = "ok"
        }
    | n > 1000000 = SolverIntegralOutputData {
        isIntegralSuccessfully = False
        , calculatedIntegral = -1
        , errIntegralVal = -1
        , partsCount = n
        , integralInfromationMsg = "Parts count limit exceeded. Maybe integral diverges."
        }
    | otherwise = solve strategy f a b eps (n * 2) curVal
    where 
        curVal = calcRectangle strategy f a b n
        runge = abs (oldVal - curVal) / 3