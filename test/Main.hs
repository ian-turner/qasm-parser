module Main (main) where

import QasmParser


main :: IO ()
main = do
    (Program _ stmts) <- parseQasmFile "test/adder10_inlined.qasm"
    let stmts_str = map show stmts
    putStr (unlines stmts_str)
    return ()
