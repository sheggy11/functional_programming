module Colon.Parser (
    parseProgramWithDict,
    parseCommand
) where

import Colon.Core
import Data.Char (isDigit)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (isPrefixOf)

-- Проверка, является ли строка числом (включая отрицательные)
isNumber :: String -> Bool
isNumber ('-':xs) = all isDigit xs
isNumber xs = all isDigit xs

-- Парсинг команды
parseCommand :: String -> Map String Command -> Either String Command
parseCommand word dict
    | ".\"" `isPrefixOf` word = Right (parseStringLiteral (drop 2 word))  -- Обработка строки после ."
    | word == "\"" = Left "Error: Missing ending quote for string"  -- Ошибка, если нет конца строки
    | otherwise = case Map.lookup word dict of
        Just cmd -> Right cmd
        Nothing -> if isNumber word
            then Right $ push (read word)
            else Left $ "Unknown word: " ++ word

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
