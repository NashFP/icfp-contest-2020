{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Parse(Expression(..), parseFile, parse, replEntry) where

import System.IO
import Text.Parsec
import Text.Parsec.Char
import Data.Char (digitToInt)

data Expression =
  Constant Integer
  | ListLiteral [Expression]
  | Identifier String
  | Apply Expression Expression
  | BitString [Int]

type Parser t = forall s u m . Stream s m Char => ParsecT s u m t

-- Parse an identifier that starts with a letter, and consists of letters and digits.
alphaIdentifier :: Parser String
alphaIdentifier = do
  start <- letter
  part <- many (letter <|> digit)
  return $ start : part

-- Parse a special identifier like `:1234`.
numericIdentifier :: Parser String
numericIdentifier = do
  string ":"
  digits <- many1 digit
  return $ ":" ++ digits

bitstring :: Parser Expression
bitstring = do
  string "%"
  digits <- many1 digit
  return $ BitString $ map digitToInt digits
  
listLiteral :: Parser Expression
listLiteral = do
  string "( "
  many $ string " "
  expressions <- expressionSpace `sepBy` commaSpace
  string ")"
  return $ ListLiteral expressions

expressionSpace :: Parser Expression
expressionSpace = do
  expr <- expression
  many1 $ string " "
  return expr

commaSpace :: Parser [String]
commaSpace = do
  comma <- string ", "
  many $ string " "

-- Parse either kind of identifier.
identifier :: Parser String
identifier = alphaIdentifier <|> numericIdentifier

-- Parse a variable or a function application (`ap x y`).
variableOrFunctionApplication :: Parser Expression
variableOrFunctionApplication = do
  name <- identifier
  if name == "ap"
    then do string " "
            f <- expression
            string " "
            arg <- expression
            return $ Apply f arg
    else return $ Identifier name

-- Parse an integer literal that doesn't start with `-`.
nonnegativeIntegerLiteral :: Parser Expression
nonnegativeIntegerLiteral = do
  digits <- many1 digit
  return $ Constant $ read digits

-- Parse an integer literal that starts with `-`.
negativeIntegerLiteral :: Parser Expression
negativeIntegerLiteral = do
  string "-"
  digits <- many1 digit
  return $ Constant $ - read digits

-- Parse an expression (the stuff that appears on the right side of `=`).
expression :: Parser Expression
expression =
      variableOrFunctionApplication
  <|> nonnegativeIntegerLiteral
  <|> negativeIntegerLiteral
  <|> bitstring
  <|> listLiteral

-- Parse an equation (one line of "galaxy.txt").
equation :: Parser (String, Expression)
equation = do
  id <- identifier
  string " = "
  expr <- expression
  return (id, expr)

-- Parse a program (all of "galaxy.txt").
program :: Parser [(String, Expression)]
program = do
  v <- equation `sepEndBy` string "\n"
  eof
  return v

-- Parse a program, but in the IO monad.
parseProgram :: String -> String -> IO [(String, Expression)]
parseProgram filename sourceStr =
  case parse program filename sourceStr of
    Left errors -> fail $ show errors
    Right equations -> return equations

-- Read and parse a file, given the filename. Returns the environment.
parseFile :: String -> IO [(String, Expression)]
parseFile filename =
  do input <- readFile filename
     parseProgram filename input

-- Read a single expression (for the repl).
replEntry :: Parser Expression
replEntry = do
  v <- expression
  eof
  return v
