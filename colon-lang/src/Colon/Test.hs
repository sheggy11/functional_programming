module Colon.Test where

import Colon.Core
import Colon.Parser
import Colon.Interpreter

-- Тестирование базовых операций
testPushPop :: Bool
testPushPop = case execute [push 5, push 10, pop] [] of
    Right [5] -> True
    _ -> False

testAdd :: Bool
testAdd = case execute [push 3, push 4, add] [] of
    Right [7] -> True
    _ -> False

testSub :: Bool
testSub = case execute [push 5, push 3, sub] [] of
    Right [2] -> True
    _ -> False

testMul :: Bool
testMul = case execute [push 3, push 2, mul] [] of
    Right [6] -> True
    _ -> False

testDiv :: Bool
testDiv = case execute [push 4, push 2, div'] [] of
    Right [2] -> True
    _ -> False

-- Запуск всех тестов
runTests :: IO ()
runTests = do
    putStrLn $ "Test Push/Pop: " ++ show testPushPop
    putStrLn $ "Test Add: " ++ show testAdd
    putStrLn $ "Test Sub: " ++ show testSub
    putStrLn $ "Test Mul: " ++ show testMul
    putStrLn $ "Test Div: " ++ show testDiv

