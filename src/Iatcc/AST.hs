module Iatcc.AST where

data Program
  = PExpr Expr
  | PLetRec [Fun] Expr
  deriving (Eq, Ord, Show)

data Fun = Fun
  { funName :: Text
  , funParams :: [Text]
  , funBody :: Expr
  }
  deriving (Eq, Ord, Show)

data Expr
  = EConst Constant
  | EUnary UnaryOp Expr
  | EBinary BinaryOp Expr Expr
  | EIf Expr Expr Expr
  | ELet Text Expr Expr
  | EApp Text [Expr]
  | EVar Text
  deriving (Eq, Ord, Show)

data UnaryOp
  = UnaryFxAdd1
  | UnaryFxSub1
  | UnaryCharToFx
  | UnaryFxToChar
  | UnaryIsFxZero
  | UnaryIsNull
  | UnaryIsPair
  | UnaryNot
  | UnaryLogicalNot
  | UnaryIsFixnum
  | UnaryIsBoolean
  | UnaryIsChar
  | UnaryCar
  | UnaryCdr
  deriving (Eq, Ord, Show)

data BinaryOp
  = BinaryAnd
  | BinaryOr
  | BinaryAdd
  | BinarySub
  | BinaryMul
  | BinaryCons
  | BinaryLogicalAnd
  | BinaryLogicalOr
  | BinaryEqual
  | BinaryNotEqual
  | BinaryLesser
  | BinaryLesserEqual
  | BinaryGreater
  | BinaryGreaterEqual
  | BinarySeq
  | BinarySetCar
  | BinarySetCdr
  deriving (Eq, Ord, Show)

data Constant
  = CFixnum Integer -- -2^29 .. 2^29-1
  | CBoolean Bool   -- false, true
  | CChar Char      -- ASCII characters
  | CNull           -- ()
  deriving (Eq, Ord, Show)
