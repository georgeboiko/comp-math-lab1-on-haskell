module Methods.Lab4LogarithmicMethod (LogarithmicApprox(..)) where

import Types.SolverTypes
import Types.MathTypes
import Utils.MathUtils (linearCoeffs2x2, computeApproxMetrics)

data LogarithmicApprox = LogarithmicApprox

instance ApproxSolver LogarithmicApprox where
    solveApprox _ pts
        | any (\(x, _) -> x <= 0) pts = SolverApproxOutputData
            { approxIsSuccessfully = False
            , approxName = "Logarithmic"
            , approxFormula = ""
            , approxCoefficients = []
            , approxDeviationS = -1
            , approxStdDeviation = -1
            , approxDetermination = -1
            , approxPearsonR = 0
            , approxPhiValues = []
            , approxResiduals = []
            , approxErrMsg = "Logarithmic approximation requires all x > 0"
            }
        | otherwise =
            let xs = map fst pts
                lnxs = map log xs
                ys = map snd pts
                n = fromIntegral (length pts) :: Double
                sx = sum lnxs
                sy = sum ys
                sxx = sum (map (** 2) lnxs)
                sxy = sum (zipWith (*) lnxs ys)
                mat = [[sxx, sx], [sx, n]] :: Matrix
                rhs = [sxy, sy]
                (a, b) = linearCoeffs2x2 mat rhs
                phi = map (\x -> a * log x + b) xs
                (s, delta, r2, phiVals, resids) = computeApproxMetrics pts phi
            in SolverApproxOutputData
                { approxIsSuccessfully = True
                , approxName = "Logarithmic"
                , approxFormula = ""
                , approxCoefficients = [a, b]
                , approxDeviationS = s
                , approxStdDeviation = delta
                , approxDetermination = r2
                , approxPearsonR = 0
                , approxPhiValues = phiVals
                , approxResiduals = resids
                , approxErrMsg = ""
                }
