{-# LANGUAGE FlexibleContexts #-}

module Parse(parseFile) where

import Text.Parsec
import Text.Parsec.Char
import qualified Data.Map as Map
import Eval

evaluateFile filename sourceStr =
  let variables :: Map.Map String Value
      variables = Map.fromList (stdlib ++ parsedEquations)

      getValueOfVariable :: String -> Value
      getValueOfVariable key =
        case Map.lookup key variables of
          Nothing -> error ("no such variable: " ++ key)
          Just value -> value

      parsedEquations :: [(String, Value)]
      parsedEquations =
        case runParser file () filename sourceStr of
          Left errors -> error $ "parsing failed: " ++ show errors
          Right equations -> equations

      alphaIdentifier :: Stream s m Char => ParsecT s u m String
      alphaIdentifier = many1 letter

      numericIdentifier :: Stream s m Char => ParsecT s u m String
      numericIdentifier = do
        string ":"
        digits <- many1 digit
        return $ ":" ++ digits

      identifier :: Stream s m Char => ParsecT s u m String
      identifier = alphaIdentifier <|> numericIdentifier

      variableOrFunctionApplication :: Stream s m Char => ParsecT s u m Value
      variableOrFunctionApplication = do
        name <- identifier
        if name == "ap"
          then do string " "
                  f <- expression
                  string " "
                  arg <- expression
                  return $ ap f arg
          else return $ getValueOfVariable name

      nonnegativeIntegerLiteral :: Stream s m Char => ParsecT s u m Value
      nonnegativeIntegerLiteral = do
        digits <- many1 digit
        return $ IntValue $ read digits

      negativeIntegerLiteral :: Stream s m Char => ParsecT s u m Value
      negativeIntegerLiteral = do
        string "-"
        digits <- many1 digit
        return $ IntValue $ - read digits

      expression :: Stream s m Char => ParsecT s u m Value
      expression =
            variableOrFunctionApplication
        <|> nonnegativeIntegerLiteral
        <|> negativeIntegerLiteral

      equation :: Stream s m Char => ParsecT s u m (String, Value)
      equation = do
        id <- identifier
        string " = "
        expr <- expression
        return (id, expr)

      file = equation `sepEndBy` (string "\n")

  in variables


parseFile filename =
  do input <- readFile filename
     return $ evaluateFile filename input

