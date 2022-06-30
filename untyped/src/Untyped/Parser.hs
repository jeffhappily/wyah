module Untyped.Parser (
  parseExpr,
) where

import Text.Parsec (ParseError, eof, many1, parse, (<|>))
import Text.Parsec.Language (haskellStyle)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Untyped.AST (Expr (..), Lit (LInt))

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = ["->", "\\", "+", "*", "-", "="]
    names = []
    style =
      haskellStyle
        { Tok.reservedOpNames = ops
        , Tok.reservedNames = names
        , Tok.commentLine = "#"
        }

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

ident :: Parser String
ident = Tok.identifier lexer

variable :: Parser Expr
variable = Var <$> Tok.identifier lexer

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  whiteSpace
  names <- many1 ident
  reservedOp "."
  body <- expr
  return $ foldr Lam body names

number :: Parser Expr
number = Lit . LInt . fromIntegral <$> Tok.integer lexer

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

term :: Parser Expr
term =
  parens expr
    <|> variable
    <|> number
    <|> lambda

expr :: Parser Expr
expr = do
  es <- many1 term
  return (foldl1 App es)

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"
