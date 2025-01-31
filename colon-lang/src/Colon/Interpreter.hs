module Colon.Interpreter (
    execute
) where

import Colon.Core

-- Выполнение программы (список команд)
execute :: [Command] -> Stack -> ColonResult
execute [] stack = Right stack
execute (cmd:cmds) stack = do
    stack' <- cmd stack
    execute cmds stack'