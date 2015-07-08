module Numparse where

data Op = Plus | Minus | Mul | Div | Pow deriving (Eq, Show)

data Expr a = Number a
            | Symbol String
            | UnaryArith String (Expr a)
            | BinaryArith Op (Expr a) (Expr a)
            deriving (Eq)

instance Num a => Num (Expr a) where
  a + b = BinaryArith Plus a b
  a - b = BinaryArith Minus a b
  a * b = BinaryArith Mul a b
  negate a = BinaryArith Mul (Number (-1)) a
  abs a = UnaryArith "abs" a
  signum _ = error "signum not impl"
  fromInteger i = Number (fromInteger i)

instance Fractional a => Fractional (Expr a) where
  a / b = BinaryArith Div a b
  recip a = BinaryArith Div (Number 1) a
  fromRational r = Number (fromRational r)

instance Floating a => Floating (Expr a) where
  pi = Symbol "pi"
  exp a = UnaryArith "exp" a
  log a = UnaryArith "log" a
  sqrt a = UnaryArith "sqrt" a
  a ** b = BinaryArith Pow a b
  sin a = UnaryArith "sin" a
  cos a = UnaryArith "cos" a
  tan a = UnaryArith "tan" a
  asin a = UnaryArith "asin" a
  acos a = UnaryArith "acos" a
  atan a = UnaryArith "asin" a
  sinh a = UnaryArith "sinh" a
  cosh a = UnaryArith "cosh" a
  tanh a = UnaryArith "tanh" a
  asinh a = UnaryArith "asinh" a
  acosh a = UnaryArith "acosh" a
  atanh a = UnaryArith "atanh" a

instance (Show a, Num a) => Show (Expr a) where
  show x = prettyShow x

{-Printing-}
prettyShow :: (Show a, Num a) => Expr a -> String
prettyShow (Number a) = show a
prettyShow (Symbol s) = s
prettyShow (UnaryArith op expr) = op ++ "(" ++ show expr ++ ")"
prettyShow (BinaryArith op lhs rhs) =
  let lhsp = prettyParen lhs
      rhsp = prettyParen rhs
      opp  = op2str op
      in lhsp ++ opp ++ rhsp

prettyParen :: (Show a, Num a) =>Expr a ->String
prettyParen (Number x) = prettyShow (Number x)
prettyParen (Symbol s) = prettyShow (Symbol s)
prettyParen x@(UnaryArith _ _) = prettyShow x
prettyParen x@(BinaryArith _ _ _) = "(" ++ prettyShow x ++ ")"

op2str :: Op -> String
op2str Plus = "+"
op2str Minus = "-"
op2str Mul = "*"
op2str Div = "/"
op2str Pow = "^"

{-manipulation-}
simplify :: (Num a, Eq a) =>Expr a ->Expr a
simplify (BinaryArith op lhs rhs) =
  let slhs = simplify lhs
      srhs = simplify rhs
   in
   case (op, slhs, srhs) of
        (Mul, Number 1, x) ->x
        (Mul, x, Number 1) ->x
        (Mul, Number 0, _) ->Number 0
        (Mul, _, Number 0) ->Number 0
        (Plus, x, Number 0) ->x
        (Plus, Number 0, x) ->x
        (Minus, x, Number 0) -> x
        (Minus, Number 0, x) ->x
        (Div, x, Number 1) ->x
        _ -> BinaryArith op slhs srhs
simplify (UnaryArith _ expr) = simplify expr
simplify x = x




