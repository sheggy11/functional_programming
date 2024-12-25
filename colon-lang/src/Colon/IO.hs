module Colon.IO (
    printStack,
    inputValue
) where

import Colon.Core
import Debug.Trace (trace)

-- Вывод стека
printStack :: Command
printStack stack = Right (trace (show stack) stack)

-- Добавление значения в стек через ввод (эмуляция)
inputValue :: Int -> Command
inputValue x = push x

