module Infer where
import           Annotate
import           Constraint (constr)
import           Parser     (parseExpr)
import           Solve      (solver, subExpr)
import           TypeSyntax
infer :: String -> Expr

infer s = let
            parsed = case parseExpr s of
                       Right e -> e
            typed_tree = annotate parsed
            constraints = constr typed_tree
            sub = solver constraints
            subbedTree = subExpr sub typed_tree
           in
            subbedTree
