module QasmParser (
    parseQasmFile
) where

import Text.Parsec
import Text.Parsec.Char (digit, letter, string)
import Text.Parsec.Combinator (many1, sepBy)

import Qasm


parseQasmFile :: String -> IO Program
parseQasmFile file = do
    return (Program "OPENQASM 3.0" [] [])
