module Qasm where


data Expr = Plus Expr Expr
            | Minus Expr Expr
            | Times Expr Expr
            | Div Expr Expr
            | Bracket Expr Expr
            | Negate Expr Expr
            | Euler 
            | Pi
            | Tau
            | DecFloat String
            | DecInt String
            | QasmId String
            | QasmCell String Expr
            | QasmMeasure GateOperand
            deriving (Show, Eq)

data GateOperand = QVar String
                   | QReg String Expr
                   deriving (Show, Eq)

data GateExpr = Gate String [Expr] [GateOperand]
            deriving (Show, Eq)

data LValue = CVar String
              | CReg String Expr
              deriving (Show, Eq)

data Type = BitT
          | BitArrT Expr
          | QubitT
          | QubitArrT Expr
          deriving (Show, Eq)

type Version = String

data Stmt = QasmGateStmt GateExpr
            | QasmDeclStmt Type String
            | QasmAssignStmt LValue Expr
            | QasmInitDeclStmt Type String Expr
            deriving (Show, Eq)

data QasmInclude = QasmInclude String deriving (Show, Eq)

data Program = Program Version [QasmInclude] [Stmt] deriving (Show, Eq)
