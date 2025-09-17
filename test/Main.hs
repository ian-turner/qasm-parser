module Main (main) where

import Parser


main :: IO ()
main = do
    (Program _ stmts) <- parseQasmFile "tmp/adder10_inlined.qasm"
    let stmts_str = map show stmts
    putStr (unlines stmts_str)
    return ()
