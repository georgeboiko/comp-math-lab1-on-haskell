module Methods.Lab4CubicMethod (CubicApprox(..)) where

import Types.SolverTypes
import Utils.MathUtils (computeApproxMetrics, gaussianSolve)

data CubicApprox = CubicApprox

instance ApproxSolver CubicApprox where
    solveApprox _ pts =
        let xs = map fst pts
            ys = map snd pts
            n = length pts
            sx = sum xs
            sx2 = sum (map (** 2) xs)
            sx3 = sum (map (** 3) xs)
            sx4 = sum (map (** 4) xs)
            sx5 = sum (map (** 5) xs)
            sx6 = sum (map (** 6) xs)
            sy = sum ys
            sxy = sum (zipWith (*) xs ys)
            sx2y = sum (zipWith (\x y -> x ** 2 * y) xs ys)
            sx3y = sum (zipWith (\x y -> x ** 3 * y) xs ys)
            mat = [ [fromIntegral n, sx, sx2, sx3]
                  , [sx, sx2, sx3, sx4]
                  , [sx2, sx3, sx4, sx5]
                  , [sx3, sx4, sx5, sx6]
                  ]
            rhs = [sy, sxy, sx2y, sx3y]
            coeffs = gaussianSolve mat rhs
            a0 = coeffs !! 0
            a1 = coeffs !! 1
            a2 = coeffs !! 2
            a3 = coeffs !! 3
            phi = map (\x -> a0 + a1 * x + a2 * x ** 2 + a3 * x ** 3) xs
            (s, delta, r2, phiVals, resids) = computeApproxMetrics pts phi
        in SolverApproxOutputData
            { approxIsSuccessfully = True
            , approxName = "Cubic"
            , approxFormula = ""
            , approxCoefficients = coeffs
            , approxDeviationS = s
            , approxStdDeviation = delta
            , approxDetermination = r2
            , approxPearsonR = 0
            , approxPhiValues = phiVals
            , approxResiduals = resids
            , approxErrMsg = ""
            }
