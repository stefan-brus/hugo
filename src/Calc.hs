-- Infix calculator module

module Calc where

import Numeric

import Control.Applicative ((<$>))

import Text.ParserCombinators.Parsec

-- Expression type
data Expr =
    Number Double
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Show)

----------------------------
-- CALCULATOR LOGIC FUNCTIONS --
----------------------------

-- Evaluate an expression
evalExpr :: Expr -> Double
evalExpr (Number n) = n
evalExpr (Add e1 e2) = evalExpr e1 + evalExpr e2
evalExpr (Sub e1 e2) = evalExpr e1 - evalExpr e2
evalExpr (Mul e1 e2) = evalExpr e1 * evalExpr e2
evalExpr (Div e1 e2) = evalExpr e1 / evalExpr e2

-- Calculate the value of the given string
calculate :: String -> Either ParseError Double
calculate s = evalExpr <$> parse expr "" s

-- Build an expression tree from the given list of operator/expression pairs
foldExpr :: Expr -> [(Char,Expr)] -> Expr
foldExpr e = foldl build e
  where
    build :: Expr -> (Char,Expr) -> Expr
    build res (op,ex) = case op of
      '+' -> Add res ex
      '-' -> Sub res ex
      '*' -> Mul res ex
      '/' -> Div res ex
      _ -> error "Unexpected operator"

-- Parse an expression
expr :: Parser Expr
expr = do
  whitespace
  term

-- Parse a term
term :: Parser Expr
term = do
  t1 <- factor
  ops <- many $ try termOp
  return $ foldExpr t1 ops
  where
    termOp :: Parser (Char,Expr)
    termOp = do
      whitespace
      op <- oneOf "+-"
      whitespace
      t2 <- factor
      return (op,t2)

-- Parse a factor
factor :: Parser Expr
factor = do
  n1 <- primary
  ops <- many $ try facOp
  return $ foldExpr n1 ops
  where
    facOp :: Parser (Char,Expr)
    facOp = do
      whitespace
      op <- oneOf "*/"
      whitespace
      n2 <- primary
      return (op,n2)

-- Parse a primary
primary :: Parser Expr
primary = try parens <|> number

-- Parse an expression in parentheses
parens :: Parser Expr
parens = do
  neg <- option "" $ return <$> char '-'
  whitespace
  skip '('
  whitespace
  res <- expr
  whitespace
  skip ')'
  return $ if null neg then res else Mul (Number (-1)) res

-- Parse a number
number :: Parser Expr
number = do
  neg <- option "" $ return <$> char '-'
  int <- many1 digit
  com <- option "" $ return <$> char '.'
  dec <- if null com then return "" else many1 digit
  return $ Number . head $ fst <$> (readSigned readFloat $ neg ++ int ++ com ++ dec)

----------------------
-- HELPER FUNCTIONS --
----------------------

-- Consume whitespace
whitespace :: Parser ()
whitespace = skipMany $ oneOf " \n\r\t"

-- Skip a character
skip :: Char -> Parser ()
skip c = do
  _ <- char c
  return ()
