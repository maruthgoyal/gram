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
applyOneSub :: Sub -> Expr -> Expr
applyOneSub _ (Lit l) = (Lit l)
applyOneSub (x, y) (Var name) | x == name = y
                              | otherwise = (Var name)

applyOneSub s (App e1 e2) = (App (applyOneSub s e1) (applyOneSub s e2))

applyOneSub (x, t') (Lam y t) | x == y = (Lam y t)
                              | otherwise =
                                      let
                                              new_name = rename y (Set.union (fv t) (fv t'))
                                              renamed_body = applyOneSub (y, (Var new_name)) t
                                              final_body = applyOneSub (x, t') renamed_body
                                      in
                                              (Lam new_name final_body)

applySub :: Substitution -> Expr -> Expr
applySub [] e     = e
applySub (x:xs) e = applySub xs (applyOneSub x e)

-- Composes 2 substitutions s1 and s2. Applies s2 to the range of s1.
composeSub :: Substitution -> Substitution -> Substitution
composeSub s1 s2 = [(s, (applySub s2 e)) | (s, e) <- s1]
