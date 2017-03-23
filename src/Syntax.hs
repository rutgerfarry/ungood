module Syntax where

type Name = String

data Expr
  = IntValue Integer
  | Var TypeSpecifier Name
  | BinOp Op Expr Expr
  | Call Name [Expr]
  | Function TypeSpecifier Name [Expr] Expr
  deriving (Eq, Ord, Show)

data TypeSpecifier
  = Int
  | Void
  deriving (Eq, Ord, Show)

data Op
  = Plus
  | Minus
  | Times
  | Divide
  deriving (Eq, Ord, Show)
