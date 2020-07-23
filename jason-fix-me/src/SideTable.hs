{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module SideTable(load, empty, applySideTable) where

import System.IO
import Parse
import Text.Parsec
import Text.Parsec.Char
import qualified Data.Map as M

type Parser t = forall s u m . Stream s m Char => ParsecT s u m t

data SideTable = SideTable { entries :: M.Map String String }

type Entry = (String, String)

-- minimum unit of whitespace
singleSpace :: Parser ()
singleSpace = oneOf " \t" >> return ()

-- any line can end with a comment
lineComment :: Parser ()
lineComment = string "--" >> many (noneOf "\r\n") >> return ()

-- optional whitespace
ows :: Parser ()
ows = many singleSpace >> return ()

-- whitespace
ws :: Parser ()
ws = many1 singleSpace >> return ()

-- end of line
eol :: Parser ()
eol = optional lineComment >> optional (char '\r') >> char '\n' >> return ()

blankLine :: Parser [Entry]
blankLine = ows >> eol >> return []

line :: Parser [Entry]
line = do
  left <- ows >> identifier
  ows >> char '='
  right <- ows >> identifier
  ows
  many (identifier >> ows)  -- arguments
  eol
  return [(left, right)] -- discard the arguments

file :: Parser SideTable
file = do
  lists <- many (line <|> blankLine)
  return $ SideTable $ M.fromList $ concat lists

-- Read and parse a file
load :: String -> IO SideTable
load filename = do
  input <- readFile filename
  case parse file filename input of
    Left errors -> fail $ show errors
    Right entries -> return entries


empty = SideTable M.empty

applySideTable :: SideTable -> [(String, Expression)] -> [(String, Expression)]
applySideTable table pairs = map (\(s, e) -> (lookup s, apply e)) pairs
  where
    lookup :: String -> String
    lookup name = maybe name id $ M.lookup name (entries table)
    apply :: Expression -> Expression
    apply e@(Constant _) = e
    apply (ListLiteral exprs) = ListLiteral (map (apply) exprs)
    apply (Identifier name) = Identifier (lookup name)
    apply (Apply f arg) = Apply (apply f) (apply arg)
    apply e@(BitString bits) = e
