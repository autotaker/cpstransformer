module Parser where
import Control.Applicative hiding((<|>))
import TypeCheck(Symbol(..),Sort(..), Term(..))

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language(haskellDef)
import qualified Data.Map as M
import Text.Parsec.String

lexer :: P.TokenParser st
lexer = P.makeTokenParser haskellDef

identifier :: Parser String
identifier = P.identifier lexer
parens :: Parser t -> Parser t
parens = P.parens lexer
reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer
symbol :: String -> Parser String
symbol = P.symbol lexer

expr , prim, app, abst :: M.Map String Symbol -> Parser Term
expr env = abst env <|> app env

app env = prim env `chainl1` pure Apply
prim env = var <|> parens (expr env) where
    var = do
        x <- identifier 
        case M.lookup x env of
            Just s -> return $ Var s
            Nothing -> unexpected $ "Not in scope: " ++ x

sortP :: Parser Sort
sortP = e `chainr1` ((:->) <$ reservedOp "->") where
    e = parens sortP <|> (O <$ symbol "o")
    
abst env = do
    reservedOp "\\"
    sym <- parens $ do
        x <- identifier
        reservedOp "::"
        ty <- sortP
        return $ Symbol x ty
    reservedOp "->"
    Abst sym <$> expr (M.insert (name sym) sym env)

term :: Parser Term
term = expr M.empty

