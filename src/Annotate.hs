module Annotate (getType, annotate) where

import           Control.Monad.State
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import qualified Syntax              as AST
import           Type
import qualified TypeSyntax          as TY


genTv :: String -> Type
genTv s = TVar (TV s)

getTvName :: TVar -> String
getTvName (TV s) = s

getType :: TY.Expr -> Type
getType e = case e of
              (TY.Lit t _)      -> t
              (TY.Var t _)      -> t
              (TY.Lam t _ _)    -> t
              (TY.App t _ _)    -> t
              (TY.Asg t _ _ _)  -> t
              (TY.IfEl t _ _ _) -> t
              (TY.Fix t _)      -> t
              (TY.Op t _ _ _)   -> t

freshName :: State Int String
freshName = do
  n <- get
  put (n + 1)
  return ("t" ++ show n)

instantiate :: Scheme -> State Int Type
instantiate (ForAll [] t) = return t
instantiate (ForAll (x:xs) t) = do
  name <- freshName
  let ty = genTv name
  let s' = apply (Map.singleton (TV x) ty) (ForAll xs t)
  instantiate s'

generalize :: Type -> TyEnv -> Scheme
generalize ty tenv = ForAll vars ty
                      where
                        fvars = Set.difference (ftv ty) (ftv tenv)
                        vars = Set.toList $ Set.map getTvName fvars

annot :: AST.Expr -> TyEnv -> State Int TY.Expr
annot (AST.Lit l@(AST.LInt _)) t = return $ TY.Lit TInt l
annot (AST.Lit l@(AST.LBool _)) t = return $ TY.Lit TBool l

annot (AST.Var v) (TyEnv env) =
  case Map.lookup v env of
    Nothing   -> error ("Unbound variable found: " ++ v)
    (Just ty) -> do
      ty' <- instantiate ty
      return $ TY.Var ty' v

annot (AST.Asg var val body) t@(TyEnv env) = do
  val' <- annot val t
  let t' = TyEnv $ Map.insert var (generalize (getType val') t) env
  body' <- annot body t'
  name <- freshName
  return $ TY.Asg (genTv name) var val' body'



annot (AST.Lam x e) (TyEnv t) = do
  name <- freshName
  let fresh_ty = genTv name
  let t' = Map.insert x (ForAll [] fresh_ty) t
  e' <- annot e (TyEnv t')
  let ty = TFunc fresh_ty (getType e')
  return $ TY.Lam ty x e'

annot (AST.App e1 e2) t = do
  name <- freshName
  e1' <- annot e1 t
  e2' <- annot e2 t
  return $ TY.App (genTv name) e1' e2'

annot (AST.IfEl e1 e2 e3) t = do
  name <- freshName
  e1' <- annot e1 t
  e2' <- annot e2 t
  e3' <- annot e3 t
  return $ TY.IfEl (genTv name) e1' e2' e3'

annot (AST.Op bo e1 e2) t = do
  e1' <- annot e1 t
  e2' <- annot e2 t
  case bo of
    AST.Eql -> return $ TY.Op TBool bo e1' e2'
    _       ->  return $ TY.Op TInt bo e1' e2'

annotate :: AST.Expr -> TY.Expr
annotate e = evalState (annot e (TyEnv Map.empty)) 0
