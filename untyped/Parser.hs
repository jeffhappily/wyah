module Parser (
  parseExpr
) where

import AST

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef, haskellStyle)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Data.Functor.Identity

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where ops = ["->","\\","+","*","-","="]
        names = []
        style = haskellStyle {Tok.reservedOpNames = ops,
                              Tok.reservedNames = names,
                              Tok.commentLine = "#"}

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

prefixOp :: String -> (a -> a) -> Ex.Operator String () Identity a
prefixOp s f = Ex.Prefix (reservedOp s >> return f)

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

ident :: Parser String
ident = Tok.identifier lexer


-- Prefix operators
table :: Ex.OperatorTable String () Identity Expr
table = [
    [
      -- prefixOp "let" Let
    ]
  ]

var :: Parser Expr
var = Var <$> Tok.identifier lexer

-- if/then/else
lambda :: Parser Expr
-- lambda = do
--   reservedOp "\\"
--   whiteSpace
--   name <- many1 ident
--   reservedOp "->"
--   body <- expr
--   return foldr
lambda = undefined

-- Constants
true, false :: Parser Expr
true  = reserved "true"  >> return (Lit (LBool True))
false = reserved "false" >> return (Lit (LBool False))
-- zero  = reservedOp "0"   >> return Zero


integerLiteral :: Parser Expr
integerLiteral = (Lit . LInt . fromIntegral) <$> Tok.integer lexer

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

factor :: Parser Expr
factor =
      true
  <|> false
  <|> integerLiteral
  <|> parens expr
  <|> lambda
  <|> var

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = semiSep expr

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s