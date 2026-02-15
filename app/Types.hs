module Types (Matrix, Vector, InputData(..), OutputData(..)) where

type Matrix = [[Double]]
type Vector = [Double]

data InputData = InputData 
    { _n :: Int
    , _matrix :: Matrix
    , _vector :: Vector
    , _eps :: Double
    }

instance Show InputData where
    show input =
        "Размерность: " ++ show (_n input) ++ "\n" ++
        "Матрица:\n"   ++ show (_matrix input) ++ "\n" ++
        "Вектор:\n"    ++ show (_vector input) ++ "\n" ++
        "eps: "        ++ show (_eps input) ++ "\n"

data OutputData = OutputData
    { _isSuccess :: Bool
    , _ansVector :: Vector
    , _errVector :: Vector
    , _iters :: Int
    , _norm :: Double
    }

instance Show OutputData where
    show output = 
        "Норма матрицы: " ++ show (_norm output) ++ "\n" ++
        "Вектор неизвестных:\n"   ++ show (_ansVector output) ++ "\n" ++
        "Вектор погрешностей:\n"    ++ show (_errVector output) ++ "\n" ++
        "Количество итераций: "        ++ show (_iters output) ++ "\n"