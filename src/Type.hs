module Type where

import qualified Data.Map   as Map
import           Data.Maybe
import qualified Data.Set   as Set

newtype TVar = TV String deriving (Show, Eq, Ord)
newtype TyEnv = TyEnv (Map.Map String Scheme)

data Type = TVar TVar
          | TInt
          | TBool
          | TFunc Type Type
          deriving (Eq, Ord)

instance Show Type where
  show (TVar (TV s)) = s
  show TInt          = "Int"
  show TBool         = "Bool"
  show (TFunc t1 t2) = (show t1) ++ " -> " ++ (show t2)

data Scheme = ForAll [String] Type

type Subst = Map.Map TVar Type

class Substitutable a where
  ftv :: a -> Set.Set TVar
  apply :: Subst -> a -> a

instance Substitutable Type where
  apply s TInt = TInt
  apply s TBool = TBool
  apply s (TVar v) = fromMaybe (TVar v) (Map.lookup v s)
  apply s (TFunc t1 t2) = TFunc t1' t2'
                          where
                            t1' = apply s t1
                            t2' = apply s t2


  ftv TInt          = Set.empty
  ftv TBool         = Set.empty
  ftv (TVar s)      = Set.singleton s
  ftv (TFunc t1 t2) = Set.union (ftv t1) (ftv t2)

instance Substitutable Scheme where
  apply s (ForAll xs t) = (ForAll xs t')
                          where
                            xs' = map TV xs
                            s' = foldr Map.delete s xs'
                            t' = apply s' t
  ftv (ForAll xs t) = Set.difference (ftv t) (Set.fromList xs')
                      where
                        xs' = map TV xs

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv   = foldr (Set.union . ftv) Set.empty


instance Substitutable TyEnv where
  apply s (TyEnv t)  = (TyEnv (Map.map (apply s) t))
  ftv (TyEnv t) = ftv $ Map.elems t
