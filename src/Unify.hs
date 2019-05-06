module Unify where

import           Data.Maybe
import           Syntax
import           Type

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set



occurs :: TVar -> Type -> Bool
occurs s t = Set.member s (ftv t)

unifyVar :: TVar -> Type -> Subst
unifyVar v1 (TVar v2) | v1 == v2 = Map.empty
                      | otherwise = Map.singleton v1 (TVar v2)

unifyVar v1 ty | occurs v1 ty = error "FAILED OCCURS CHECK"
               | otherwise = Map.singleton v1 ty


compose :: Subst -> Subst -> Subst
compose s s' = Map.map (apply s') s `Map.union` s

unify :: Type -> Type -> Subst

unify TInt TInt = Map.empty
unify TBool TBool = Map.empty
unify (TVar var) ty = unifyVar var ty
unify ty t@(TVar var) = unifyVar var ty
unify (TFunc t1 t2) f2@(TFunc t1' t2') = compose s1 (unify s_t2 s_t2')
                                         where
                                           s1 = unify t1 t1'
                                           s_t2 = apply s1 t2
                                           s_t2' = apply s1 t2'

unify t1 t2 = error "NON UNIFIABLE TYPES"
