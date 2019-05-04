module Eval where

import           Syntax

import qualified Data.Set as Set

type Sub = (String, Expr)
type Substitution = [Sub]

fv' :: Expr -> Set.Set String -> Set.Set String
fv' (Var x) s         = Set.insert x s
fv' (App e1 e2) s     = Set.union (fv' e1 s) (fv' e2 s)
fv' (Lam name body) s = Set.delete name (fv' body s)

fv :: Expr -> Set.Set String
fv e = fv' e Set.empty

rename :: String -> Set.Set String -> String
rename s set | Set.notMember s set = s
             | otherwise = rename (s ++ "'") set

applyOneSub :: Sub -> Expr -> Expr
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
