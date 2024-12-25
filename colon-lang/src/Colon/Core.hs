module Colon.Core (
    Stack,
    Command,
    ColonResult,
    push,
    pop,
    add,
    sub,
    mul,
    div',
    mod',
    gt,
    lt,
    eq,
    invert,
    andOp,
    orOp,
    dup,
    swap
) where

type Stack = [Int]
type ColonResult = Either String Stack
type Command = Stack -> ColonResult

-- Добавить значение в стек
push :: Int -> Command
push x stack = Right (x : stack)

-- Удалить верхний элемент из стека
pop :: Command
pop [] = Left "Error: Stack underflow"
pop (_ : xs) = Right xs

-- Сложение
add :: Command
add (x:y:xs) = Right ((x + y) : xs)
add _ = Left "Error: Not enough elements on stack for addition"

-- Вычитание
sub :: Command
sub (x:y:xs) = Right ((y - x) : xs)
sub _ = Left "Error: Not enough elements on stack for subtraction"

-- Умножение
mul :: Command
mul (x:y:xs) = Right ((x * y) : xs)
mul _ = Left "Error: Not enough elements on stack for multiplication"

-- Деление
div' :: Command
div' (x:y:xs)
    | x == 0    = Left "Error: Division by zero"
    | otherwise = Right ((y `div` x) : xs)
div' _ = Left "Error: Not enough elements on stack for division"

-- Остаток от деления (MOD)
mod' :: Command
mod' (x:y:xs)
    | x == 0    = Left "Error: Division by zero in MOD"
    | otherwise = Right ((y `mod` x) : xs)
mod' _ = Left "Error: Not enough elements on stack for MOD"


-- Дублирование
dup :: Command
dup (x:xs) = Right (x : x : xs)
dup _ = Left "Error: Stack underflow for duplication"

-- Обмен двух верхних элементов
swap :: Command
swap (x:y:xs) = Right (y : x : xs)
swap _ = Left "Error: Not enough elements on stack for swap"

-- Больше
gt :: Command
gt (x:y:xs) = Right ((if y > x then 1 else 0) : xs)
gt _ = Left "Error: Not enough elements on stack for comparison (>)"

-- Меньше
lt :: Command
lt (x:y:xs) = Right ((if y < x then 1 else 0) : xs)
lt _ = Left "Error: Not enough elements on stack for comparison (<)"

-- Равно
eq :: Command
eq (x:y:xs) = Right ((if y == x then 1 else 0) : xs)
eq _ = Left "Error: Not enough elements on stack for comparison (==)"

-- Логическое И (AND)
andOp :: Command
andOp (x:y:xs) = Right ((if (y /= 0) && (x /= 0) then -1 else 0) : xs) -- -1 represents True
andOp _ = Left "Error: Not enough elements on stack for AND"

-- Логическое ИЛИ (OR)
orOp :: Command
orOp (x:y:xs) = Right ((if (y /= 0) || (x /= 0) then -1 else 0) : xs) -- -1 represents True
orOp _ = Left "Error: Not enough elements on stack for OR"

-- Инвертирование (INVERT)
invert :: Command
invert (x:xs) = Right ((if x == 0 then -1 else 0) : xs) -- -1 represents True, 0 represents False
invert _ = Left "Error: Stack underflow for INVERT"

