module Parser (parseExpr) where

import           Text.Parsec
import           Text.Parsec.Language (haskellStyle)
import           Text.Parsec.String   (Parser)

import qualified Text.Parsec.Expr     as Ex
import qualified Text.Parsec.Token    as Tok

import           Syntax

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where ops = ["->","\\","+","*","-","="]
        names = []
        style = haskellStyle {Tok.reservedOpNames = ops,
                              Tok.reservedNames = names,
                              Tok.commentLine = "#"}

identifier :: Parser String
identifier = Tok.identifier lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

lambda :: Parser Expr
lambda = do
        reservedOp "\\"
        arguments <- many1 identifier
        reservedOp "->"
        body <- expr
        return $ foldr Lam body arguments

number :: Parser Expr
number = do
        n <- Tok.natural lexer
        return (Lit (LInt (fromIntegral n)))

variable :: Parser Expr
variable = do
        name <- identifier
        return (Var name)

parens :: Parser a -> Parser a
parens = Tok.parens lexer

term :: Parser Expr
term = parens expr
        <|> number
        <|> variable
        <|> lambda

expr :: Parser Expr
expr = do
        exprs <- many1 term
        return $ foldl1 App exprs

contents :: Parser a -> Parser a
contents p = do
        Tok.whiteSpace lexer
        e <- p
        eof
        return e

parseExpr :: String -> Either ParseError Expr
parseExpr input = parse (contents expr) "<stdin>" input
