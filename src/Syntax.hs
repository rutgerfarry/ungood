module Syntax where

type Name = String

data Expr
  = Int Integer 
  | BinOp Op Expr Expr
  | Var String
  | Call Name [Expr]
  | Function Name [Expr] Expr
  | Extern Name [Expr]
  deriving (Eq, Ord, Show)

data Op
  = Plus
  | Minus
  | Times
  | Divide
  deriving (Eq, Ord, Show)

