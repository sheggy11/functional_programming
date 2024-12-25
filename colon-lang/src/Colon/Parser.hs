module Colon.Parser (
    parseCommand,
    parseProgram
) where

import Colon.Core
import Data.Char (isDigit)

-- Проверка, является ли строка числом (включая отрицательные)
isNumber :: String -> Bool
isNumber ('-':xs) = all isDigit xs  -- Отрицательное число
isNumber xs = all isDigit xs        -- Положительное число

-- Парсинг команды
parseCommand :: String -> Command
parseCommand "+" = add
parseCommand "-" = sub
parseCommand "*" = mul
parseCommand "/" = div'
parseCommand "MOD" = mod'
parseCommand "dup" = dup
parseCommand "swap" = swap
parseCommand ">" = gt
parseCommand "<" = lt
parseCommand "=" = eq
parseCommand "and" = andOp
parseCommand "or" = orOp
parseCommand "invert" = invert
parseCommand num
    | isNumber num = push (read num)  -- Поддержка отрицательных чисел
    | otherwise = const (Left ("Unknown command: " ++ num))

-- Парсинг программы (строка в список команд)
parseProgram :: String -> [Command]
parseProgram input = map parseCommand (words input)
