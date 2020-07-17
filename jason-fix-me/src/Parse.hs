{-# LANGUAGE FlexibleContexts #-}

module Parse(parseFile, repl, Env) where

import System.IO
import Text.Parsec
import Text.Parsec.Char
import qualified Data.Map as Map
import Eval

-- The type of environments. An environment is basically a collection of
-- name-value pairs.
type Env = Map.Map String Value

-- Parse an identifier made of all letters.
alphaIdentifier :: Stream s m Char => ParsecT s u m String
alphaIdentifier = many1 letter

-- Parse a special identifier like `:1234`.
numericIdentifier :: Stream s m Char => ParsecT s u m String
numericIdentifier = do
  string ":"
  digits <- many1 digit
  return $ ":" ++ digits

-- Parse either kind of identifier.
identifier :: Stream s m Char => ParsecT s u m String
identifier = alphaIdentifier <|> numericIdentifier

-- Look up the value of a variable in an environment.
getValueOfVariable :: Env -> String -> Value
getValueOfVariable variables key =
  case Map.lookup key variables of
    Nothing -> error ("no such variable: " ++ key)
    Just value -> value

-- Parse a variable or a function application (`ap x y`).
variableOrFunctionApplication :: Stream s m Char => Env -> ParsecT s u m Value
variableOrFunctionApplication env = do
  name <- identifier
  if name == "ap"
    then do string " "
            f <- expression env
            string " "
            arg <- expression env
            return $ ap f arg
    else return $ getValueOfVariable env name

-- Parse an integer literal that doesn't start with `-`.
nonnegativeIntegerLiteral :: Stream s m Char => ParsecT s u m Value
nonnegativeIntegerLiteral = do
  digits <- many1 digit
  return $ IntValue $ read digits

-- Parse an integer literal that starts with `-`.
negativeIntegerLiteral :: Stream s m Char => ParsecT s u m Value
negativeIntegerLiteral = do
  string "-"
  digits <- many1 digit
  return $ IntValue $ - read digits

-- Parse an expression (the stuff that appears on the right side of `=`).
expression :: Stream s m Char => Env -> ParsecT s u m Value
expression env =
      variableOrFunctionApplication env
  <|> nonnegativeIntegerLiteral
  <|> negativeIntegerLiteral

-- Parse an equation (one line of "galaxy.txt").
equation :: Stream s m Char => Env -> ParsecT s u m (String, Value)
equation env = do
  id <- identifier
  string " = "
  expr <- expression env
  return (id, expr)

-- Parse a program (all of "galaxy.txt").
program :: Stream s m Char => Env -> ParsecT s u m [(String, Value)]
program env = equation env `sepEndBy` (string "\n")

-- Parse a program, returning the environment.
evaluateFile :: String -> String -> Env
evaluateFile filename sourceStr =
  let variables :: Env
      variables = Map.fromList (stdlib ++ parsedEquations)

      parsedEquations :: [(String, Value)]
      parsedEquations =
        case runParser (program variables) () filename sourceStr of
          Left errors -> error $ "parsing failed: " ++ show errors
          Right equations -> equations

  in variables

-- Read and parse a file, given the filename. Returns the environment.
parseFile :: String -> IO Env
parseFile filename =
  do input <- readFile filename
     return $ evaluateFile filename input


repl :: Env -> IO ()
repl env = do
  putStr "\x1b[36m\x1b[1m*>\x1b[0m "
  hFlush stdout
  line <- getLine
  case runParser (expression env) () "<stdin>" line of
    Left errors -> putStrLn $ "syntax error: " ++ show errors
    Right value -> putStrLn $ show value
  repl env
