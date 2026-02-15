module Readers (readFromConsole, readFromFile, readForGeneration) where

import Generators
import Writers (writeMatrix, writeVector)
import Types
import Text.Read (readMaybe)
import System.IO
import System.Directory (doesFileExist)
import Control.Exception (try, SomeException, displayException)

sanitize :: String -> String
sanitize = map (\c -> if c == ',' then '.' else c)

readTyped :: Read a => Handle -> String -> IO a
readTyped h errorMsg = do
    isTerminal <- hIsTerminalDevice h
    line <- hGetLine h
    case readMaybe (sanitize line) of 
        Just n -> return n
        Nothing -> if isTerminal
            then do
                hPutStrLn stderr errorMsg
                readTyped h errorMsg
            else
                error errorMsg

readInt :: Handle -> IO Int
readInt h = readTyped h "Ошибка: введите int!"

readDouble :: Handle -> IO Double
readDouble h = readTyped h "Ошибка: введите double!"

readNumbers :: [String] -> Maybe [Double]
readNumbers [] = Just []
readNumbers (x:xs) =
    case readMaybe x of
        Just n ->
            case readNumbers xs of
                Just ns -> Just (n : ns)
                Nothing -> Nothing
        Nothing -> Nothing

readRow :: Handle -> Int -> IO [Double]
readRow _ 0 = return []
readRow h n = do
    line <- hGetLine h
    let parts = words line
    let cleanParts = map sanitize parts
    isTerminal <- hIsTerminalDevice h
    if length cleanParts /= n
        then do
            if isTerminal then do
                hPutStrLn stderr "Ошибка: неверное количество чисел"
                readRow h n
            else 
                error "Ошибка в файле: неверное количество чисел в строке"
        else
            case readNumbers cleanParts of
                Just row -> return row
                Nothing -> do
                    if isTerminal then do
                        hPutStrLn stderr "Ошибка: в строке есть не числа"
                        readRow h n
                    else 
                        error "Ошибка в файле: в строке есть не числа"

readVector :: Handle -> Int -> IO Vector
readVector h n = do readRow h n

readMatrix :: Handle -> Int -> Int -> IO Matrix
readMatrix _ 0 _ = return []
readMatrix h n cols = do
    row <- readRow h cols
    other <- readMatrix h (n-1) cols
    return $ row : other

readAllData :: Handle -> IO InputData
readAllData h = do
    n <- readInt h
    matrix <- readMatrix h n n
    vector <- readVector h n
    eps <- readDouble h
    return InputData
        { _n = n
        , _matrix = matrix
        , _vector = vector
        , _eps = eps
        }

readFromConsole :: IO InputData
readFromConsole = do
    putStrLn "Введите n, матрицу, вектор и eps:"
    readAllData stdin

readFromFile :: IO InputData
readFromFile = do
    putStrLn "Введите имя файла:"
    filename <- getLine
    exists <- doesFileExist filename
    if not exists
        then do
            putStrLn $ "Ошибка: Файл " ++ filename ++ " не найден."
            readFromFile
        else do
            result <- try (withFile filename ReadMode readAllData) :: IO (Either SomeException InputData)  
            case result of
                Left ex -> do
                    putStrLn $ head (lines (displayException ex))
                    readFromFile
                Right inputData -> do
                    writeMatrix (_matrix inputData) "Матрица:"
                    writeVector (_vector inputData) "b" "Вектор:"
                    return inputData
            

readForGeneration :: IO InputData
readForGeneration = do
    putStrLn "Введите n, eps:"
    n <- readInt stdin
    eps <- readDouble stdin

    matrix <- generateMatrix n n
    vector <- generateVector n

    writeMatrix matrix "Сгенерированная матрица:"
    writeVector vector "b" "Сгенерированный вектор:"

    return InputData
        { _n = n
        , _matrix = matrix
        , _vector = vector
        , _eps = eps
        }