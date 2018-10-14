module Parser (parseProp) where
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative hiding ((<|>), many)
import Ast

parseLit :: Parser PExpr
parseLit = string "(Var" *> (Var <$> (spaces *> many letter <* spaces)) <* char ')'

parseNot :: Parser PExpr
parseNot = string "(Not" *> (Not <$> (spaces *> parseProp <* spaces)) <* char ')'

parseAnd :: Parser PExpr
parseAnd = string "(And" *> (And <$> (spaces *> parseProp <* spaces) <*> (spaces *> parseProp <* spaces)) <* char ')'

parseOr :: Parser PExpr
parseOr = string "(Or" *> (Or <$> (spaces *> parseProp <* spaces) <*> (spaces *> parseProp <* spaces)) <* char ')'

parseImp :: Parser PExpr
parseImp = string "(Imp" *> (Imp <$> (spaces *> parseProp <* spaces) <*> (spaces *> parseProp <* spaces)) <* char ')'


parseBi :: Parser PExpr 
parseBi = string "(Bi" *> (Bi <$> (spaces *> parseProp <* spaces) <*> (spaces *> parseProp <* spaces)) <* char ')'


parseProp :: Parser PExpr
parseProp = try parseLit <|> try parseAnd <|> try parseOr
        <|> try parseImp <|> try parseBi <|> error "Syntactiall wrong formula"

{-
 *Parser> parse parseProp  "" "(And (Or (Var p) (Var q)) (Var w))"
Right (And (Or (Var "p") (Var "q")) (Var "w"))
*Parser> parse parseProp  "" "(Imp (And (Or (Var p) (Var q)) (Var w)) (And (Or (Var p) (Var q)) (Var w)))"
Right (Imp (And (Or (Var "p") (Var "q")) (Var "w")) (And (Or (Var "p") (Var "q")) (Var "w")))
*Parser> parse parseProp  "" "(Imp (And (Or (Var p) (Var q)) (Var w)) (And (Or (Var p) (Var q)) (Var w))"
*** Exception: Syntactiall wrong formula
CallStack (from HasCallStack):
  error, called at /Users/keep_learning/Mukesh/Github/Typelevel-Haskell/Haskell/propositional-logic/src/Parser.hs:33:46 in main:Parser
*Parser> parse parseProp  "" "(Imp (And (Or (Var p) (Var q)) (Var w)) (And (Or (Var p) (Var q)) (Var w)))"
Right (Imp (And (Or (Var "p") (Var "q")) (Var "w")) (And (Or (Var "p") (Var "q")) (Var "w")))
*Parser>
-}


