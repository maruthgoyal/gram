module Syntax where

data Expr
        = Lit Lit
        | Var String
        | Lam String Expr
        | App Expr Expr
        deriving (Eq, Show)

data Lit
        = LInt Int
        | LBool Bool
        deriving (Show, Eq, Ord)
