module Colon.Utils (
    condition,
    ifElse,
    doLoop
) where

import Colon.Core

-- Пример условия: проверка положительности вершины стека
condition :: Stack -> Bool
condition (x:_) = x > 0
condition _ = False

-- Условный оператор
ifElse :: (Stack -> Bool) -> Command -> Command -> Command
ifElse cond trueBranch falseBranch stack =
    if cond stack
    then trueBranch stack
    else falseBranch stack

-- Цикл DO I LOOP
doLoop :: Int -> Int -> Command -> Command
doLoop start end cmd stack
    | start >= end = Right stack
    | otherwise = do
        stack' <- cmd stack
        doLoop (start + 1) end cmd stack'


