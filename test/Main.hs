module Main (main) where

import Parser


main :: IO ()
main = do
    pr <- parseQasmFile "tmp/adder10_inlined.qasm"
    putStrLn $ show pr
    return ()
