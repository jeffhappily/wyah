module Typed.Parser where

import Typed.Eval

import Data.Functor (($>))
import Text.Parsec
import Text.Parsec.Language (haskellStyle)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    names = ["if", "then", "else", "pred", "succ", "iszero", "true", "false"]
    style = haskellStyle {Tok.reservedNames = names}

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

reservedNames :: String -> Parser ()
reservedNames = Tok.reserved lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

ident :: Parser String
ident = Tok.identifier lexer

zero :: Parser Expr
zero = reservedNames "0" $> Zero

true :: Parser Expr
true = reservedNames "true" $> Tr

false :: Parser Expr
false = reservedNames "false" $> Fl

succ' :: Parser Expr
succ' =
  reservedNames "succ"
    *> (Succ <$> term)

pred' :: Parser Expr
pred' =
  reservedNames "pred"
    *> (Pred <$> term)

isZero :: Parser Expr
isZero =
  reservedNames "iszero"
    *> (IsZero <$> term)

ifThenElse :: Parser Expr
ifThenElse = do
  _ <- reservedNames "if"
  clause <- term
  _ <- reservedNames "then"
  e1 <- term
  _ <- reservedNames "else"
  e2 <- term

  return $ If clause e1 e2

-- | Clear white space before and EOF after parser
contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

-- | All possible @Expr@
term :: Parser Expr
term =
  zero
    <|> true
    <|> false
    <|> succ'
    <|> pred'
    <|> isZero
    <|> ifThenElse
    <|> parens term

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents term) "<stdin>"
