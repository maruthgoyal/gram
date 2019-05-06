module Syntax where

import qualified Data.Map as Map

data Expr
        = Lit Lit
        | Var String
        | Lam String Expr
        | App Expr Expr
        | Asg String Expr Expr
        | IfEl Expr Expr Expr
        | Fix Expr
        | Op Binop Expr Expr
        deriving (Eq, Show)

data Lit
        = LInt Int
        | LBool Bool
        deriving (Show, Eq, Ord)

data Binop = Add | Sub | Mul deriving (Show, Eq, Ord)

