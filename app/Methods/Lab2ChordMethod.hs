module Methods.Lab2ChordMethod (ChordMethod (..)) where
    
import Utils.EquationStorage
import Utils.MathUtils
import Types.SolverTypes (SolverEquationOutputData(..), EquationSolver(..))

data ChordMethod = ChordMethod

instance EquationSolver ChordMethod where
    solveEquation _ eq a b eps 
        | not (hasRoot (equation eq) a b)  = SolverEquationOutputData {
         isSucessfully = False
        , calculatedAns = -1
        , informationMsg =  "No root on interval"
        , iterationsCnt = 0
        }
        | not (isUniqueRoot (equation eq) (equation' eq) a b 0.01) = SolverEquationOutputData {
            isSucessfully = False
            , calculatedAns = -1
            , informationMsg =  "Root not unique or function not monotonic"
            , iterationsCnt = 0
            }
        | otherwise = solve (equation eq) a b eps a 1

solve :: (Double -> Double) -> Double -> Double -> Double -> Double -> Int -> SolverEquationOutputData
solve eq a b eps x_old iters
    | abs (x_n - x_old) < eps = SolverEquationOutputData {
         isSucessfully = True
        , calculatedAns = x_n
        , informationMsg = "ok"
        , iterationsCnt = iters
        }
    | otherwise = solve eq a1 b1 eps x_n (iters + 1)
    where
        x_n = a - (eq a) * (b - a) / (eq b - eq a)
        (a1, b1) = if (eq a) * (eq x_n) < 0 
                   then (a, x_n) 
                   else (x_n, b)
