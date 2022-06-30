module Parser (
  parseExpr,
) where

import Syntax (Expr (App, Lam, Lit, Var), Lit (LBool, LInt))
import Text.Parsec (ParseError, eof, many1, parse, (<|>))
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.Language (haskellStyle)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Type (Type (..))

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = ["->", "\\", "+", "*", "-", "=", ":"]
    names = []
    style =
      haskellStyle
        { Tok.reservedOpNames = ops
        , Tok.reservedNames = names
        , Tok.commentLine = "#"
        }

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

ident :: Parser String
ident = Tok.identifier lexer

variable :: Parser Expr
variable = Var <$> Tok.identifier lexer

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  name <- ident
  reservedOp ":"
  ty <- type'
  reservedOp "."
  body <- expr
  return $ Lam name ty body

bool :: Parser Expr
bool =
  (reserved "True" >> return (Lit (LBool True)))
    <|> (reserved "False" >> return (Lit (LBool False)))

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
    <|> bool
    <|> variable
    <|> number
    <|> lambda

expr :: Parser Expr
expr = do
  es <- many1 term
  return (foldl1 App es)

tyatom :: Parser Type
tyatom = tylit <|> parens type'

tylit :: Parser Type
tylit = (reservedOp "Bool" >> return TBool) <|> (reservedOp "Int" >> return TInt)

type' :: Parser Type
type' = Ex.buildExpressionParser tyops tyatom
  where
    infixOp x f = Ex.Infix (reservedOp x >> return f)
    tyops =
      [ [infixOp "->" TArr Ex.AssocRight]
      ]

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"
