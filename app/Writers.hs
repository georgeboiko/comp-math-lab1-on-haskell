module Writers where

import Types
import Text.Printf (printf)

formatDoubleWithPrecision :: Double -> Int -> String
formatDoubleWithPrecision num precision = printf "%.*f" precision num

formatDouble :: Double -> String
formatDouble num = formatDoubleWithPrecision num 6

formatVector :: [Double] -> String -> [String]
formatVector vector name = [name ++ "_" ++ show (i :: Int) ++ " = " ++ formatDouble val | (i, val) <- zip [1..] vector]

formatMatrixRow :: [Double] -> String
formatMatrixRow row = "| " ++ unwords (map formatDouble row) ++ " |"

writeToConsole :: OutputData -> IO ()
writeToConsole output = do
    if (_isSuccess output) then do
        putStr "Норма матрицы: "
        putStrLn $ formatDouble (_norm output)

        writeVector (_ansVector output) "x" "Вектор неизвестных:"

        putStr "Количество итераций: "
        putStrLn $ show (_iters output)

        writeVector (_errVector output) "err" "Вектор погрешностей:"        
    else
        putStrLn "Ошибка: Не удалось достичь диагонального преобладания.\n"    

writeVector :: Vector -> String -> String -> IO ()
writeVector vector name title = do
    putStrLn title
    putStrLn $ unlines $ formatVector vector name

writeMatrix :: Matrix -> String -> IO ()
writeMatrix matrix title = do
    putStrLn title
    putStrLn $ unlines $ map formatMatrixRow matrix