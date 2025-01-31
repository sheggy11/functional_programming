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
    printStringLiteral,
    sToF,
    fToS,
    Value(..),
    create,       
    cells,       
    allot        
) where

import System.IO (hFlush, stdout)
import Control.Monad.IO.Class (liftIO)
import System.IO.Unsafe (unsafePerformIO)  
import Control.Exception (SomeException, catch)  

-- тип Value для поддержки Int и Float
data Value = I Int | F Float | Array [Value]  
    deriving (Show, Eq, Read)

instance Num Value where
    (I x) + (I y) = I (x + y)
    (F x) + (F y) = F (x + y)
    (I x) + (F y) = F (fromIntegral x + y)
    (F x) + (I y) = F (x + fromIntegral y)

    (I x) * (I y) = I (x * y)
    (F x) * (F y) = F (x * y)
    (I x) * (F y) = F (fromIntegral x * y)
    (F x) * (I y) = F (x * fromIntegral y)

    abs (I x) = I (abs x)
    abs (F x) = F (abs x)

    signum (I x) = I (signum x)
    signum (F x) = F (signum x)

    fromInteger x = I (fromInteger x)  -- Позволяет писать `0`, `1`, `2` в коде

    negate (I x) = I (negate x)
    negate (F x) = F (negate x)

instance Ord Value where
    (I x) `compare` (I y) = x `compare` y
    (F x) `compare` (F y) = x `compare` y
    (I x) `compare` (F y) = (fromIntegral x) `compare` y
    (F x) `compare` (I y) = x `compare` (fromIntegral y)


type Stack = [Value]
type ColonResult = Either String Stack
type Command = Stack -> ColonResult


-- Функция для выполнения IO в контексте Either String
liftIOtoEither :: IO a -> Either String a
liftIOtoEither action = unsafePerformIO (fmap Right action `catch` handler)
  where
    handler :: SomeException -> IO (Either String a)
    handler _ = return (Left "Error in IO action")


-- Операции над стеком

push :: Value -> Command
push x stack = Right (x : stack)

pop :: Command
pop [] = Left "Error: Stack underflow"
pop (_ : xs) = Right xs

printStringLiteral :: String -> Command
printStringLiteral str stack = do
    liftIOtoEither (putStrLn str)
    return stack


-- Арифметические операции (работают с I и F)
add, sub, mul, div' :: Command
add (x:y:xs) = Right (op (+) x y : xs)
add _ = Left "Error: Not enough elements on stack for addition"

sub (x:y:xs) = Right (op (-) y x : xs)
sub _ = Left "Error: Not enough elements on stack for subtraction"

mul (x:y:xs) = Right (op (*) x y : xs)
mul _ = Left "Error: Not enough elements on stack for multiplication"

div' (x:y:xs)
    | toFloat x == 0 = Left "Error: Division by zero"
    | otherwise = Right (op (/) y x : xs)
div' _ = Left "Error: Not enough elements on stack for division"

mod' :: Command
mod' (I x:I y:xs)
    | x == 0    = Left "Error: Division by zero in MOD"
    | otherwise = Right (I (y `mod` x) : xs)
mod' _ = Left "Error: MOD only works with integers"



-- CREATE создает новую переменную/массив
create :: String -> Int -> Command
create name size stack = Right (Array (replicate size (I 0)) : stack)

sizeOfCell :: Int
sizeOfCell = 4  

-- CELLS 
cells :: Command
cells (I n : xs) = Right (I (n * sizeOfCell) : xs)  
cells _ = Left "Error: Not enough elements on stack for CELLS"

-- ALLOT выделяет память для массива
allot :: Command
allot (I n : Array arr : xs) = Right (Array (replicate n (I 0)) : xs)
allot _ = Left "Error: ALLOT requires an array"


-- Запись в массив (store)
store :: Command
store (I val : I index : Array arr : xs) = 
    if index < length arr
        then Right (Array (take index arr ++ [I val] ++ drop (index + 1) arr) : xs)
        else Left "Error: Index out of bounds"
store _ = Left "Error: Invalid arguments for store"

-- Чтение из массива (fetch)
fetch :: Command
fetch (I index : Array arr : xs) = 
    if index < length arr
        then Right (arr !! index : xs)
        else Left "Error: Index out of bounds"
fetch _ = Left "Error: Invalid arguments for fetch"

-- Пример операции с массивом: +! (сложение и запись)
plusStore :: Command
plusStore (I val : I index : Array arr : xs) = 
    if index < length arr
        then let (I oldVal) = arr !! index
             in Right (Array (take index arr ++ [I (oldVal + val)] ++ drop (index + 1) arr) : xs)
        else Left "Error: Index out of bounds"
plusStore _ = Left "Error: Invalid arguments for +!"


-- Операции сравнения

gt, lt, eq :: Command
gt (x:y:xs) = Right (boolToValue (y > x) : xs)
gt _ = Left "Error: Not enough elements on stack for comparison (>)"

lt (x:y:xs) = Right (boolToValue (y < x) : xs)
lt _ = Left "Error: Not enough elements on stack for comparison (<)"

eq (x:y:xs) = Right (boolToValue (y == x) : xs)
eq _ = Left "Error: Not enough elements on stack for comparison (==)"


-- Логические операции

andOp, orOp, invert :: Command
andOp (x:y:xs) = Right (boolToValue (toBool x && toBool y) : xs)
andOp _ = Left "Error: Not enough elements on stack for AND"

orOp (x:y:xs) = Right (boolToValue (toBool x || toBool y) : xs)
orOp _ = Left "Error: Not enough elements on stack for OR"

invert (x:xs) = Right (boolToValue (not (toBool x)) : xs)
invert _ = Left "Error: Stack underflow for INVERT"

-- Операции над стеком
dup, swap, drop', over, rot :: Command
dup (x:xs) = Right (x:x:xs)
dup _ = Left "Error: Stack underflow for duplication"

swap (x:y:xs) = Right (y:x:xs)
swap _ = Left "Error: Not enough elements on stack for swap"

drop' [] = Left "Error: Stack underflow for DROP"
drop' (_:xs) = Right xs

over (x:y:xs) = Right (y:x:y:xs)
over _ = Left "Error: Not enough elements on stack for OVER"

rot (x:y:z:xs) = Right (z:x:y:xs)
rot _ = Left "Error: Not enough elements on stack for ROT"

-- Конвертация между Int и Float
sToF :: Command
sToF (I n:xs) = Right (F (fromIntegral n) : xs)
sToF (F _:xs) = Right (xs)
sToF _ = Left "Error: Stack underflow for S>F"

fToS :: Command
fToS (F f:xs) = Right (I (floor f) : xs)
fToS (I _:xs) = Right (xs)
fToS _ = Left "Error: Stack underflow for F>S"

-- Вспомогательные функции
op :: (Float -> Float -> Float) -> Value -> Value -> Value
op f (I x) (I y) = I (floor (f (fromIntegral y) (fromIntegral x)))
op f a b = F (f (toFloat a) (toFloat b))

toFloat :: Value -> Float
toFloat (I n) = fromIntegral n
toFloat (F d) = d

toBool :: Value -> Bool
toBool (I 0) = False
toBool (I _) = True
toBool (F f) = f /= 0.0

boolToValue :: Bool -> Value
boolToValue True = I (-1)
boolToValue False = I 0

-- Ввод/вывод
dot :: Command
dot (x:xs) = do
    liftIOtoEither (print x)
    return xs
dot _ = Left "Error: Not enough elements on stack for ."

cr :: Command
cr stack = do
    liftIOtoEither (putStrLn "")
    return stack

emit :: Command
emit (I x:xs) = do
    liftIOtoEither (putChar (toEnum x :: Char))
    liftIOtoEither (hFlush stdout)
    return xs
emit _ = Left "Error: Not enough elements on stack for EMIT"

key :: Command
key stack = do
    c <- liftIOtoEither getChar
    return (I (fromEnum c) : stack)