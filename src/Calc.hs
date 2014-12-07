{-# OPTIONS_GHC -Wall #-}

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

-- Parse an expression
expr :: Parser Expr
expr = do
  whitespace
  try term <|> try factor <|> number

-- Parse a term
term :: Parser Expr
term = do
  t1 <- try factor <|> number
  whitespace
  op <- oneOf "+-"
  whitespace
  t2 <- try term <|> try factor <|> number
  if op == '+' then return $ Add t1 t2 else return $ Sub t1 t2

-- Parse a factor
factor :: Parser Expr
factor = do
  n1 <- number
  whitespace
  op <- oneOf "*/"
  whitespace
  n2 <- try factor <|> number
  if op == '*' then return $ Mul n1 n2 else return $ Div n1 n2

-- Parse a number
number :: Parser Expr
number = do
  neg <- option "" $ return <$> char '-'
  int <- many1 digit
  com <- option "" $ return <$> char '.'
  dec <- if null com then return "" else many1 digit
  return $ Number . head $ fst <$> (readSigned readFloat $ neg ++ int ++ com ++ dec)

-- Consume whitespace
whitespace :: Parser ()
whitespace = skipMany $ oneOf " \n\r\t"
