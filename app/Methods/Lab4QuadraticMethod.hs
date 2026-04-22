module Methods.Lab4QuadraticMethod (QuadraticApprox(..)) where

import Types.SolverTypes
import Utils.MathUtils (computeApproxMetrics)
import Methods.Lab1SimpleIterationsMethod (SystemSimpleItersMethod(..))

data QuadraticApprox = QuadraticApprox

instance ApproxSolver QuadraticApprox where
    solveApprox _ pts =
        let xs = map fst pts
            ys = map snd pts
            n = length pts
            sx = sum xs
            sx2 = sum (map (** 2) xs)
            sx3 = sum (map (** 3) xs)
            sx4 = sum (map (** 4) xs)
            sy = sum ys
            sxy = sum (zipWith (*) xs ys)
            sx2y = sum (zipWith (\x y -> x ** 2 * y) xs ys)
            mat = [[fromIntegral n, sx, sx2],
                   [sx, sx2, sx3],
                   [sx2, sx3, sx4]
                  ]
            rhs = [sy, sxy, sx2y]
            result = solveLinearSystem SystemSimpleItersMethod mat rhs 1e-10
            coeffs = linCalculatedVector result
            a0 = coeffs !! 0
            a1 = coeffs !! 1
            a2 = coeffs !! 2
            phi = map (\x -> a0 + a1 * x + a2 * x ** 2) xs
            (s, delta, r2, phiVals, resids) = computeApproxMetrics pts phi
        in SolverApproxOutputData
            { approxIsSuccessfully = linIsSystemSucessfully result
            , approxName = "Quadratic"
            , approxFormula = ""
            , approxCoefficients = coeffs
            , approxDeviationS = s
            , approxStdDeviation = delta
            , approxDetermination = r2
            , approxPearsonR = 0
            , approxPhiValues = phiVals
            , approxResiduals = resids
            , approxErrMsg = linInformationSystemMsg result
            }
