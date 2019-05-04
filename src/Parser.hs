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

-- Parses a single identifier
identifier :: Parser String
identifier = Tok.identifier lexer

-- Parses a single reserved operation
reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

-- Parses a Lambda Abstraction (Function)
lambda :: Parser Expr
lambda = do
        reservedOp "\\"
        arguments <- many1 identifier
        reservedOp "->"
        body <- expr
        return $ foldr Lam body arguments

-- Parses a single natural number
number :: Parser Expr
number = do
        n <- Tok.natural lexer
        return (Lit (LInt (fromIntegral n)))

-- Parses an identifier, and creates a variable
variable :: Parser Expr
variable = do
        name <- identifier
        return (Var name)

-- Finds a pair of parentheses
parens :: Parser a -> Parser a
parens = Tok.parens lexer

-- Parses one term
term :: Parser Expr
term = parens expr
        <|> number
        <|> variable
        <|> lambda

-- Parses one expression
expr :: Parser Expr
expr = do
        exprs <- many1 term
        return $ foldl1 App exprs

-- Skips whitespace, applies the parser, finds the end-of-file
contents :: Parser a -> Parser a
contents p = do
        Tok.whiteSpace lexer
        e <- p
        eof
        return e

parseExpr :: String -> Either ParseError Expr
parseExpr input = parse (contents expr) "<stdin>" input
