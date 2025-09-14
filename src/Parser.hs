{-# LANGUAGE LambdaCase #-}
module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (emptyDef)
import System.Environment (getArgs)

-- ===== AST =====
data Header = Header3 deriving (Eq, Show)

data QType = Qubit | Bit deriving (Eq, Show)

data VarRef = VarRef
  { vrName  :: String
  , vrIndex :: Maybe Int          -- q           or q[0]
  } deriving (Eq, Show)

data Stmt
  = Include FilePath
  | Decl QType String (Maybe Int)      -- qubit[2] q;  | bit[2] c;
  | GateCall String [VarRef]           -- x q;  | cx q[0], q[1];
  | MeasureAssign VarRef VarRef        -- c[0] = measure q[0];
  | Reset VarRef                       -- reset q[1];
  deriving (Eq, Show)

data Program = Program Header [Stmt] deriving (Eq, Show)

-- ===== Lexer =====
lang :: Tok.LanguageDef ()
lang = emptyDef
  { Tok.identStart      = letter <|> char '_'
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.reservedNames   = ["OPENQASM","include","measure","reset"]
    -- NOTE: don't reserve "qubit"/"bit" so they can still be identifiers if desired.
  , Tok.reservedOpNames = ["="]  -- we use "=" in measurement assignments
  , Tok.commentLine     = "//"
  , Tok.commentStart    = "/*"
  , Tok.commentEnd      = "*/"
  , Tok.caseSensitive   = True
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser lang

identifier = Tok.identifier   lexer
reserved   = Tok.reserved     lexer
symbol     = Tok.symbol       lexer
stringLit  = Tok.stringLiteral lexer
integer    = Tok.integer      lexer
natural    = Tok.natural      lexer
semi       = Tok.semi         lexer
whiteSpace = Tok.whiteSpace   lexer
commaSep   = Tok.commaSep     lexer

-- helpful bracket parsers (explicitly, to avoid bracket token quirks)
lbrack, rbrack :: Parser String
lbrack = symbol "["
rbrack = symbol "]"

bracketedInt :: Parser Int
bracketedInt = between lbrack rbrack (fromInteger <$> integer)

-- ===== Parsers =====
program :: Parser Program
program = whiteSpace *> (Program <$> header <*> many stmt) <* eof

header :: Parser Header
header = do
  reserved "OPENQASM"
  v <- natural
  _ <- semi
  if v == 3 then pure Header3
            else fail "Only 'OPENQASM 3;' is supported in this minimal parser."

stmt :: Parser Stmt
stmt =  try includeStmt
    <|> try declStmt
    <|> try measureStmt     -- put before gateCall (both start with identifier)
    <|> try resetStmt
    <|> gateCallStmt

includeStmt :: Parser Stmt
includeStmt = do
  reserved "include"
  fp <- stringLit
  _ <- semi
  pure (Include fp)

-- OpenQASM 3: size belongs to the TYPE: qubit[2] q;  bit[2] c;
declStmt :: Parser Stmt
declStmt = do
  qt <-  (symbol "qubit" *> pure Qubit)
     <|> (symbol "bit"   *> pure Bit)
  msize <- optionMaybe bracketedInt
  name  <- identifier
  _ <- semi
  pure (Decl qt name msize)

-- Measurement-as-assignment: c[0] = measure q[0];
measureStmt :: Parser Stmt
measureStmt = try $ do
  dst <- varRef <?> "classical destination (e.g., c or c[0])"
  _   <- symbol "="
  reserved "measure"
  src <- varRef <?> "qubit to measure (e.g., q or q[0])"
  _ <- semi
  pure (MeasureAssign dst src)

resetStmt :: Parser Stmt
resetStmt = do
  reserved "reset"
  v <- varRef
  _ <- semi
  pure (Reset v)

gateCallStmt :: Parser Stmt
gateCallStmt = do
  gname <- identifier <?> "gate name (e.g., x, cx)"
  args  <- commaSep varRef <?> "argument list (e.g., q or q[0], q[1])"
  _ <- semi
  pure (GateCall gname args)

varRef :: Parser VarRef
varRef = do
  name <- identifier
  mIx  <- optionMaybe bracketedInt
  pure (VarRef name mIx)

parseQasmFile :: String -> IO Program
parseQasmFile file = do
    src <- readFile file
    case parse program file src of
        Left err  -> error $ show err
        Right ast -> return ast
