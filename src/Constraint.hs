module Constraint where

import           Annotate   (getType)
import qualified Syntax     as AST
import           Type
import           TypeSyntax

type Constr = (Type, Type)

constr ::  Expr -> [Constr]

constr (Lit _ _)      = []
constr (Var _ _)      = []

constr (Lam _ _ body) = constr body

constr (IfEl ty con true false) = [(ty, getType true),
                                   (getType con, TBool),
                                   (getType true, getType false)]
                                   ++ constr con
                                   ++ constr true
                                   ++ constr false

constr (App ty e1 e2) = [(getType e1, TFunc (getType e2) ty)]
                        ++ constr e1
                        ++ constr e2

constr (Asg ty _ e1 e2) = [(ty, getType e2)]
                          ++ constr e1
                          ++ constr e2

constr (Op _ AST.Eql e1 e2) = [(getType e1, getType e2)]
                                 ++ constr e1
                                 ++ constr e2
constr (Op _ _ e1 e2) = [(getType e1, TInt),
                               (getType e2, TInt)]
                               ++ constr e1
                               ++ constr e2
