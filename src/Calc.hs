-- Infix calculator module

module Calc where

import Data.Fixed (mod')

import Numeric

import Control.Applicative ((<$>))

import Text.ParserCombinators.Parsec

-----------
-- TYPES --
-----------

data Expr =
    Number Double
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Mod Expr Expr
  | Pow Expr Expr
  deriving (Show)

--------------------------------
-- CALCULATOR LOGIC FUNCTIONS --
--------------------------------

-- Evaluate an expression
evalExpr :: Expr -> Double
evalExpr (Number n) = n
evalExpr (Add e1 e2) = evalExpr e1 + evalExpr e2
evalExpr (Sub e1 e2) = evalExpr e1 - evalExpr e2
evalExpr (Mul e1 e2) = evalExpr e1 * evalExpr e2
evalExpr (Div e1 e2) = evalExpr e1 / evalExpr e2
evalExpr (Mod e1 e2) = evalExpr e1 `mod'` evalExpr e2
evalExpr (Pow e1 e2) = evalExpr e1 ** evalExpr e2

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
      '^' -> Pow res ex
      '%' -> Mod res ex
      _ -> error "Unexpected operator"

-- Parse an expression
expr :: Parser Expr
expr = do
  whitespace
  term

-- Parse a term
term :: Parser Expr
term = do
  f1 <- factor
  ops <- many $ try termOp
  return $ foldExpr f1 ops
  where
    termOp :: Parser (Char,Expr)
    termOp = do
      whitespace
      op <- oneOf "+-"
      whitespace
      f2 <- factor
      return (op,f2)

-- Parse a factor
factor :: Parser Expr
factor = do
  n1 <- power
  ops <- many $ try facOp
  return $ foldExpr n1 ops
  where
    facOp :: Parser (Char,Expr)
    facOp = do
      whitespace
      op <- oneOf "*/%"
      whitespace
      n2 <- power
      return (op,n2)

-- Parse a power
power :: Parser Expr
power = do
  t1 <- primary
  ops <- many $ try powerOp
  return $ transformPower $ foldExpr t1 ops
  where
    powerOp :: Parser (Char,Expr)
    powerOp = do
      whitespace
      op <- char '^'
      whitespace
      t2 <- primary
      return (op,t2)

    -- Make expression right-associative
    transformPower :: Expr -> Expr
    transformPower (Pow p@(Pow _ _) e3) = let (Pow e1 e2) = transformPower p in Pow e1 $ transformPower (Pow e2 e3)
    transformPower e = e

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
