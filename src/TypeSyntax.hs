module TypeSyntax where

import qualified Data.Map as Map
import           Syntax   (Binop, Lit)
import           Type

data Expr
        = Lit Type Lit
        | Var Type String
        | Lam Type String Expr
        | App Type Expr Expr
        | Asg Type String Expr Expr
        | IfEl Type Expr Expr Expr
        | Fix Type Expr
        | Op Type Binop Expr Expr
        deriving (Eq, Show)

