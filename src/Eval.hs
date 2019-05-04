module Eval where

import           Syntax

import qualified Data.Set as Set

type Sub = (String, Expr)
type Substitution = [Sub]

-- Helper function to find free vars.
fv' :: Expr -> Set.Set String -> Set.Set String
fv' (Var x) s         = Set.insert x s
fv' (App e1 e2) s     = Set.union (fv' e1 s) (fv' e2 s)
fv' (Lam name body) s = Set.delete name (fv' body s)

-- Finds the free variables in a given Expression
fv :: Expr -> Set.Set String
fv e = fv' e Set.empty

-- Finds a new name which is not contained in the given set
rename :: String -> Set.Set String -> String
rename s set | Set.notMember s set = s
             | otherwise = rename (s ++ "'") set

-- Application of one unit substitution to an Expression
applyOneSub :: Set.Set String -> Sub -> Expr -> Expr
applyOneSub _ _ lit@(Lit l) = lit
applyOneSub _ (x, y) (Var name) | x == name = y
                              | otherwise = Var name

applyOneSub c s (App e1 e2) = App (applyOneSub c s e1) (applyOneSub c s e2)

applyOneSub c (x, t') f@(Lam y t) | x == y = f
                              | otherwise =
                                      let
                                              c' = Set.insert y c
                                              fv_t = fv t
                                              fv_t' = fv t'
                                              fv_t_U_fv_t' = Set.difference (Set.union fv_t fv_t') c'
                                              new_name = rename y fv_t_U_fv_t'  -- fresh name
                                              renamed_body = applyOneSub c' (y, Var new_name) t -- [z/x] e
                                              final_body = applyOneSub c' (x, t') renamed_body -- [t'/x] e
                                      in
                                              Lam new_name final_body

applySub :: Set.Set String ->  Substitution -> Expr -> Expr
applySub _ [] e     = e
applySub c (x:xs) e = applySub c xs (applyOneSub c x e)

-- Composes 2 substitutions s1 and s2. Applies s2 to the range of s1.
composeSub :: Substitution -> Substitution -> Substitution
composeSub s1 s2 = [(s, applySub Set.empty s2 e) | (s, e) <- s1]

-- Applies beta-substitution to the AST
eval :: Expr -> Expr

eval (App (Lam x t) e2)       = eval $ applyOneSub (Set.fromList [x]) (x, eval e2) t
eval (App t@(App e1' e2') e2) = eval (App (eval t) e2)
-- e1 is Var or Lit now
eval t@(App e1 e2)            = t
-- Just a lambda def, or a var/lit
eval (Lam x t)                = Lam x (eval t)
eval e = e

