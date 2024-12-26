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
    swap,
    drop',
    over,
    rot,
    dot,
    cr,
    emit,
    key,
    liftIOtoEither,
    printStringLiteral
) where

import System.IO (hFlush, stdout)
import Control.Monad.IO.Class (liftIO)
import System.IO.Unsafe (unsafePerformIO)  -- Добавьте этот импорт
import Control.Exception (SomeException, catch)  -- Добавьте этот импорт для обработки исключений


type Stack = [Int]
type ColonResult = Either String Stack
type Command = Stack -> ColonResult

-- Вспомогательная функция для выполнения IO в контексте Either String
liftIOtoEither :: IO a -> Either String a
liftIOtoEither action = unsafePerformIO (fmap Right action `catch` handler)
  where
    handler :: SomeException -> IO (Either String a)
    handler _ = return (Left "Error in IO action")
-- Добавить значение в стек
push :: Int -> Command
push x stack = Right (x : stack)

-- Удалить верхний элемент из стека
pop :: Command
pop [] = Left "Error: Stack underflow"
pop (_ : xs) = Right xs

-- Печать строки (с кавычками)
printStringLiteral :: String -> Command
printStringLiteral str stack = do
    liftIOtoEither (putStrLn str)  -- Вывод строки
    return stack

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

-- DROP: удалить верхний элемент
drop' :: Command
drop' [] = Left "Error: Stack underflow for DROP"
drop' (_ : xs) = Right xs

-- OVER: дублировать предпоследний элемент на вершине
over :: Command
over (x:y:xs) = Right (y : x : y : xs)
over _ = Left "Error: Not enough elements on stack for OVER"

-- ROT: сдвинуть третий элемент наверх
rot :: Command
rot (x:y:z:xs) = Right (z : x : y : xs)
rot _ = Left "Error: Not enough elements on stack for ROT"

-- Больше
gt :: Command
gt (x:y:xs) = Right ((if y > x then -1 else 0) : xs)
gt _ = Left "Error: Not enough elements on stack for comparison (>)"

-- Меньше
lt :: Command
lt (x:y:xs) = Right ((if y < x then -1 else 0) : xs)
lt _ = Left "Error: Not enough elements on stack for comparison (<)"

-- Равно
eq :: Command
eq (x:y:xs) = Right ((if y == x then -1 else 0) : xs)
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


-- Поглощение вершины стека и вывод её
-- Поглощение вершины стека и вывод её
dot :: Command
dot (x:xs) = do
    liftIOtoEither (putStrLn (show x))  -- Используем liftIOtoEither для выполнения IO
    return xs
dot _ = Left "Error: Not enough elements on stack for ."

-- Перевод строки
cr :: Command
cr stack = do
    liftIOtoEither (putStrLn "")  -- Печать новой строки
    return stack

-- Вывод символа (по ASCII коду)
emit :: Command
emit (x:xs) = do
    liftIOtoEither (putChar (toEnum x :: Char))  -- Печать символа
    liftIOtoEither (hFlush stdout)  -- Принудительный сброс буфера для немедленного вывода
    return xs
emit _ = Left "Error: Not enough elements on stack for EMIT"

-- Ввод кода символа с клавиатуры
key :: Command
key stack = do
    c <- liftIOtoEither getChar  -- Чтение символа с клавиатуры
    return (fromEnum c : stack)  -- Положить ASCII код символа на стек





