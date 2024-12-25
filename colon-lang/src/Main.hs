module Main where

import Colon.Core
import Colon.Parser
import Colon.Interpreter
import Colon.IO
import System.IO (hFlush, stdout)
import Colon.Utils (ifElse)

-- Основной цикл программы
repl :: IO ()
repl = do
    putStr "Colon> "  -- Приглашение ввода
    hFlush stdout     -- Очистка буфера вывода
    input <- getLine  -- Чтение строки ввода
    if input == "exit"
        then putStrLn "Goodbye!"
        else do
            let commands = parseProgram input  -- Парсинг программы
            case execute commands [] of       -- Выполнение программы на пустом стеке
                Left err -> putStrLn ("Error: " ++ err) >> repl
                Right newStack -> do
                    putStrLn ("Result: " ++ show newStack)
                    repl

main :: IO ()
main = do
    putStrLn "Welcome to Colon Language REPL!"
    putStrLn "Type 'exit' to quit."
    repl  -- Запуск REPL