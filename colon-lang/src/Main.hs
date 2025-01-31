module Main where

import Colon.Core
import Colon.Parser
import Colon.Interpreter
import Colon.Utils (ifElse)
import System.Console.Haskeline  -- Для REPL с поддержкой ввода
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO (hFlush, stdout)

type Dictionary = Map String Command

-- Инициализация словаря
initialDictionary :: Dictionary
initialDictionary = Map.fromList
    [ ("+", add)
    , ("-", sub)
    , ("*", mul)
    , ("/", div')
    , ("MOD", mod')
    , ("dup", dup)
    , ("swap", swap)
    , ("drop", drop')
    , ("over", over)
    , ("rot", rot)
    , (">", gt)
    , ("<", lt)
    , ("=", eq)
    , ("and", andOp)
    , ("or", orOp)
    , ("invert", invert)
    , (".", dot)
    , ("CR", cr)
    , ("EMIT", emit)
    , ("KEY", key)
    ]

-- Функция для удаления комментариев
removeComments :: String -> String
removeComments input = go 0 input
  where
    go :: Int -> String -> String
    go _ [] = []
    go 0 ('(':xs) = go 1 xs                    -- Начало комментария
    go depth (')':xs) | depth > 0 = go (depth - 1) xs -- Конец комментария
    go depth ('(':xs) = go (depth + 1) xs      -- Вложенный комментарий
    go depth (x:xs) | depth == 0 = x : go depth xs -- Сохранение символа вне комментариев
    go depth (_:xs) = go depth xs              -- Игнорирование содержимого комментария

-- Основной цикл REPL с использованием haskeline
repl :: Dictionary -> InputT IO ()
repl dict = do
    outputStrLn "Colon> "
    input <- getInputLine ""  -- Чтение строки с поддержкой истории и автозаполнения
    case input of
        Nothing -> return ()  -- Пользователь нажал Ctrl+D (выход)
        Just "exit" -> outputStrLn "Goodbye!"  -- Выход из программы
        Just command -> do
            let cleanedInput = removeComments command  -- Удаление комментариев
            case parseAndExecute cleanedInput dict of
                Left err -> outputStrLn ("Error: " ++ err) >> repl dict
                Right (newStack, newDict) -> do
                    outputStrLn ("Result: " ++ show newStack)
                    repl newDict

-- Парсинг и выполнение команды
parseAndExecute :: String -> Dictionary -> Either String (Stack, Dictionary)
parseAndExecute input dict =
    let cleanedInput = removeComments input  -- Удаление комментариев
    in if ":" `elem` words cleanedInput
        then defineNewWord cleanedInput dict
        else executeProgram cleanedInput dict

-- Определение нового слова
defineNewWord :: String -> Dictionary -> Either String (Stack, Dictionary)
defineNewWord input dict =
    let tokens = words input
        name = tokens !! 1
        body = takeWhile (/= ";") (drop 2 tokens)
    in if null body || last tokens /= ";"
        then Left "Error: Invalid word definition syntax"
        else case compileWord body dict of
            Left err -> Left err
            Right cmd -> Right ([], Map.insert name cmd dict)

-- Компиляция нового слова
compileWord :: [String] -> Dictionary -> Either String Command
compileWord body dict = do
    commands <- parseProgramWithDict (unwords body) dict
    return $ \stack -> execute commands stack

-- Выполнение программы
executeProgram :: String -> Dictionary -> Either String (Stack, Dictionary)
executeProgram input dict =
    let commands = parseProgramWithDict input dict
    in case commands of
        Left err -> Left err
        Right cmds -> case execute cmds [] of
            Left err -> Left err
            Right stack -> Right (stack, dict)

main :: IO ()
main = do
    putStrLn "Welcome to Colon Language REPL!"
    putStrLn "Type 'exit' to quit."
    runInputT defaultSettings (repl initialDictionary)
