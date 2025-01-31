module Colon.Parser (
    parseProgramWithDict,
    parseCommand
) where

import Colon.Core
import Data.Char (isDigit)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (isPrefixOf)
import Colon.Core (Command(..), create, cells, allot, push)


-- Проверка, является ли строка числом (включая отрицательные и с плавающей точкой)
isNumber :: String -> Bool
isNumber ('-':xs) = isNumber xs  -- Обработка отрицательных чисел
isNumber xs = case span isDigit xs of
    ("", "") -> False  -- Если нет цифр до точки
    (_, rest) -> case rest of
        '.' : ys -> all isDigit ys  -- Число с плавающей точкой
        _ -> null rest  -- Если не остаток от точки, то целое число

-- Парсинг команды
parseCommand :: String -> Map String Command -> Either String Command
parseCommand word dict
    | "CREATE" `isPrefixOf` word = Right (parseCreateCommand word dict)
    | word == "CELLS" = Right cells
    | word == "ALLOT" = Right allot
    | otherwise = case Map.lookup word dict of
        Just cmd -> Right cmd
        Nothing -> if isNumber word
            then Right $ push (parseNumber word)  -- Если число, то просто кладем его на стек
            else Left $ "Unknown word: " ++ word

-- Обработка CREATE
parseCreateCommand :: String -> Map String Command -> Command
parseCreateCommand word dict = 
    let parts = words word
        name = head parts
        size = read (parts !! 1) :: Int
    in create name size


-- Функция для преобразования строки в число (Int или Float)
parseNumber :: String -> Value
parseNumber word
    | '.' `elem` word = F (read word)  -- Если есть точка, это число с плавающей точкой
    | otherwise = I (read word)  -- Иначе целое число

-- Парсинг строки для вывода
parseStringLiteral :: String -> Command
parseStringLiteral str stack = do
    let cleanedStr = takeWhile (/= '"') str  -- Ждем, что строка заканчивается кавычкой
    liftIOtoEither (putStr cleanedStr)  -- Выводим строку
    return stack

-- Утилита для обработки комментариев
skipComment :: String -> String
skipComment input =
    let (_, rest) = skipComment' 0 input
    in rest
  where
    skipComment' :: Int -> String -> (Int, String)
    skipComment' 0 [] = (0, [])
    skipComment' depth (x:xs)
        | x == '('  = skipComment' (depth + 1) xs
        | x == ')' && depth == 1 = (depth - 1, xs)
        | x == ')'  = skipComment' (depth - 1) xs
        | otherwise = skipComment' depth xs

-- Парсинг программы с учётом комментариев и словаря
parseProgramWithDict :: String -> Map String Command -> Either String [Command]
parseProgramWithDict input dict =
    traverse (`parseCommand` dict) (filter (not . null) $ parseTokens input)

-- Токенизация с учётом комментариев
parseTokens :: String -> [String]
parseTokens [] = []
parseTokens ('(':xs) = parseTokens (skipComment xs)  -- Пропустить комментарий
parseTokens xs =
    let (token, rest) = break (`elem` " \t\n") xs
    in token : parseTokens (dropWhile (`elem` " \t\n") rest)