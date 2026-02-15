module Main where

import System.Exit (exitSuccess)
import Readers (readFromConsole, readFromFile, readForGeneration)
import Writers (writeToConsole)
import Processors (processData)

main :: IO ()
main = do
    putStr "ВычМат lab №1:\nВвод с клавиатуры - 1, из файла - 2, генерация - 3\n"
    query <- getLine
    input <- case query of 
        "1" -> readFromConsole
        "2" -> readFromFile
        "3" -> readForGeneration
        "quit" -> exitSuccess
        _ -> do
            putStrLn "Неверная команда, повторите попытку..."
            main
            exitSuccess

    output <- processData input
    writeToConsole output
    main
