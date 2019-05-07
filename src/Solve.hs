module Solve where

import           Constraint
import qualified Data.Map   as Map
import           Type
import           Unify

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

