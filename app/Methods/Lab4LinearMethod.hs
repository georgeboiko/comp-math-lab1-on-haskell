module Methods.Lab4LinearMethod (LinearApprox(..)) where

import Types.SolverTypes
import Utils.MathUtils (linearCoeffs2x2, computeApproxMetrics, pearsonR)

data LinearApprox = LinearApprox

instance ApproxSolver LinearApprox where
    solveApprox _ pts =
        let xs = map fst pts
            ys = map snd pts
            n = fromIntegral (length pts) :: Double
            sx = sum xs
            sy = sum ys
            sxx = sum (map (** 2) xs)
            sxy = sum (zipWith (*) xs ys)
            mat = [[sxx, sx], [sx, n]]
            rhs = [sxy, sy]
            (a, b) = linearCoeffs2x2 mat rhs
            phi = map (\x -> a * x + b) xs
            r = pearsonR pts
            (s, delta, r2, phiVals, resids) = computeApproxMetrics pts phi
        in SolverApproxOutputData
            { approxIsSuccessfully = True
            , approxName = "Linear"
            , approxFormula = ""
            , approxCoefficients = [a, b]
            , approxDeviationS = s
            , approxStdDeviation = delta
            , approxDetermination = r2
            , approxPearsonR = r
            , approxPhiValues = phiVals
            , approxResiduals = resids
            , approxErrMsg = ""
            }
