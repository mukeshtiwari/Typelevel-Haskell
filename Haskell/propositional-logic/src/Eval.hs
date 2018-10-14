module Eval where
import Ast



{- transform implication and bi implication in terms of And, Or and Not -}
normalize :: PExpr -> PExpr
normalize (Var p) = Var p
normalize (Not p) = Not (normalize p)
normalize (Or p q) = Or (normalize p) (normalize q)
normalize (And p q) = And (normalize p) (normalize q)
normalize (Imp p q) = Or (Not (normalize p)) (normalize q)
normalize (Bi p q) = And (normalize (Imp p q)) (normalize (Imp q p))



{- https://en.wikipedia.org/wiki/Negation_normal_form -} 
nnf :: PExpr -> PExpr
nnf (Not (Or p q)) = And (nnf (Not p)) (nnf (Not q))
nnf (Not (And p q)) = Or (nnf (Not p)) (nnf (Not q))
nnf (Not (Not p)) = nnf p
nnf (And p q) = And (nnf p) (nnf q) 
nnf (Or p q) = Or (nnf p) (nnf q)
nnf e = e

normalizennf :: PExpr -> PExpr
normalizennf = nnf . normalize

{- *Eval> nnf (Imp (And (Or (Var "p") (Var "q")) (Var "w")) (And (Or (Var "p") (Var "q")) (Var "w")))
Or (Not (And (Or (Var "p") (Var "q")) (Var "w"))) (And (Or (Var "p") (Var "q")) (Var "w"))
*Eval> 
-} 

