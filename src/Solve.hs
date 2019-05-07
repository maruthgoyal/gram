module Solve where

import           Constraint
import qualified Data.Map   as Map
import           Type
import           Unify

import           TypeSyntax

solver' :: [Constr] -> Subst -> Subst
solver' [] subs = subs

solver' (x:xs) subs = let
                        t1 = fst x
                        t2 = snd x
                        newSubs = unify (apply subs t1) (apply subs t2)
                        subber = \c -> case c of
                                         (t1', t2') -> (apply subs t1', apply subs t2')
                        subsRest = map subber xs
                        in
                        solver' subsRest (compose subs newSubs)

solver :: [Constr] -> Subst
solver l = solver' l Map.empty

subExpr :: Subst -> Expr -> Expr
subExpr s l@(Lit _ _) = l
subExpr s (Var t name) = Var (apply s t) name
subExpr s (Lam t x e) = Lam (apply s t) x (subExpr s e)
subExpr s (App t e1 e2) = App (apply s t) (subExpr s e1) (subExpr s e2)
subExpr s (Asg t e e1 e2) = Asg (apply s t) e (subExpr s e1) (subExpr s e2)
subExpr s (IfEl t cond e1 e2) = IfEl (apply s t) (subExpr s cond) (subExpr s e1) (subExpr s e2)
subExpr s (Op t bop e1 e2) = Op (apply s t) bop (subExpr s e1) (subExpr s e2)
