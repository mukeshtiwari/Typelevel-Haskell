module Ast (PExpr (..))where

data PExpr = Var String 
   | Not PExpr
   | And PExpr PExpr
   | Or PExpr PExpr
   | Imp PExpr PExpr
   | Bi PExpr PExpr 
   deriving Show


