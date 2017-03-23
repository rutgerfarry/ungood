module Syntax where

type ID = String

data TypeSpecifier = Int | Void
  deriving (Eq, Ord, Show)

data Expr
  = IntValue Integer
  | VoidValue
  | VarDeclaration TypeSpecifier ID
  | BinOp Op Expr Expr
  | Call ID [Expr]
  | FunDeclaration TypeSpecifier ID [Expr] [Expr]
  deriving (Eq, Ord, Show)

data Op
  = Plus
  | Minus
  | Times
  | Divide
  deriving (Eq, Ord, Show)
