module Annotate where

import           Control.Monad.State
import qualified Data.Map.Strict     as Map
import qualified Syntax              as AST
import           Type
import qualified TypeSyntax          as TY

type TyEnv = Map.Map String Scheme

gen_tv :: String -> Type
gen_tv s = TVar (TV s)

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
  return ("t" ++ (show n))

annot :: AST.Expr -> TyEnv -> State Int TY.Expr
annot (AST.Lit l@(AST.LInt _)) t = return $ TY.Lit TInt l
annot (AST.Lit l@(AST.LBool _)) t = return $ TY.Lit TBool l
annot (AST.Var v) t = do
  name <- freshName
  return $ TY.Var (gen_tv name) v

annot (AST.Lam x e) t = do
  name <- freshName
  let fresh_ty = gen_tv name
  let t' = Map.insert x (ForAll [] fresh_ty) t
  e' <- annot e t'
  let ty = TFunc fresh_ty (getType e')
  return $ TY.Lam ty x e'

annot (AST.App e1 e2) t = do
  name <- freshName
  e1' <- annot e1 t
  e2' <- annot e2 t
  return $ TY.App (gen_tv name) e1' e2'

annot (AST.IfEl e1 e2 e3) t = do
  name <- freshName
  e1' <- annot e1 t
  e2' <- annot e2 t
  e3' <- annot e3 t
  return $ TY.IfEl (gen_tv name) e1' e2' e3'

annot (AST.Op bo e1 e2) t = do
  e1' <- annot e1 t
  e2' <- annot e2 t
  return $ TY.Op TInt bo e1' e2'

-- TODO: Create typed expressions file, import Syntax as qualified, import that as qualified too, and make this shit consistent.
-- That should do the trick.
-- Add wrapper function to evaluate state
