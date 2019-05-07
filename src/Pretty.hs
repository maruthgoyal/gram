module Pretty where

import qualified Syntax     as AST
import qualified TypeSyntax as TY
pprint' :: AST.Expr -> String
pprint' (AST.App e1 e2)     = "(" ++ (pprint' e1) ++ ") (" ++ (pprint' e2) ++ ")"
pprint' (AST.Lam x t)       = "λ" ++ x ++ ". " ++ (pprint' t)
pprint' (AST.Var x)         = x
pprint' (AST.Lit (AST.LInt x))  = show x
pprint' (AST.Lit (AST.LBool b)) = show b

pprint :: AST.Expr -> IO ()
pprint e = putStrLn (pprint' e)

tpprint' :: TY.Expr -> String
tpprint' (TY.App ty e1 e2)     = "( (" ++ (tpprint' e1) ++ ") (" ++ (tpprint' e2) ++ ") :" ++ (show ty) ++ ")"
tpprint' (TY.Lam ty x t)       = "λ: " ++ (show ty) ++ " " ++ x ++ ". " ++ (tpprint' t)
tpprint' (TY.Var ty x)         = x
tpprint' (TY.Lit ty (AST.LInt x))  = show x
tpprint' (TY.Lit ty (AST.LBool b)) = show b

