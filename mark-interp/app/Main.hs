module Main where

import Data.Either
import System.IO
import Lib
import qualified Data.Map.Strict as Map
import Data.Bits
import Debug.Trace

data AlienFunction = ADD | MUL | DIV | DEC | INC | EQUALS | LESSTHAN | MOD | DEM |
  SEND | NEG | S | C | B | POWER2 | I | CAR | CDR | ISNIL | DRAW |
  CHECKERBOARD | MULTIPLEDRAW | IFZERO | CONS | T | F
  deriving (Eq,Show)

data AlienCode =
  Apply AlienCode AlienCode |
  Func AlienFunction Int |
  CodeRef String |
  PartialFunc AlienFunction Int [AlienCode] |
  Number Integer |
  Cons AlienCode AlienCode |
  Nil |
  BitString [Int] |
  Run AlienCode
  deriving (Eq,Show)

stdFuncs = Map.fromList [
  ("add", (ADD, 2)),
  ("mul", (MUL, 2)),
  ("div", (DIV, 2)),
  ("dec", (DEC, 1)),
  ("inc", (INC, 1)),
  ("eq", (EQUALS, 2)),
  ("lt", (LESSTHAN, 2)),
  ("mod", (MOD, 1)),
  ("dem", (DEM, 1)),
  ("send", (SEND, 1)),
  ("neg", (NEG, 1)),
  ("pwr2", (POWER2,1)),
  ("i", (I,1)),
  ("s", (S,3)),
  ("c", (C,3)),
  ("b", (B,3)),
  ("t", (T,2)),
  ("f", (F,2)),
  ("car", (CAR,2)),
  ("cdr", (CDR,2)),
  ("cons", (CONS,2)),
  ("isnil", (ISNIL,1)),
  ("draw", (DRAW,1)),
  ("checkerboard", (CHECKERBOARD,2)),
  ("multipledraw", (MULTIPLEDRAW,1)),
  ("if0", (IFZERO,3))
  ]

parseLine :: String -> Either String (String,AlienCode)
parseLine l =
    let w = words l in
    let name = head w in
    case parseToken (drop 2 w) of
      Right (code, rest) ->
        if (length rest) > 0 then
          Left ("Extra tokens at end of line for function " ++ name ++ ": " ++ unwords rest)
        else
          Right (name, code)
      Left x -> Left x

isNumber (s:ss) = s == '-' || ((s >= '0') && (s <= '9'))

isCodeRef (s:ss) = s == ':'

parseToken :: [String] -> Either String (AlienCode,[String])
parseToken [] = Left "End of line reached"
parseToken (t : ts) =
  if t == "ap" then
    case parseToken ts of
      Right (e1, rest1) ->
        case parseToken rest1 of
          Right (e2, rest2) -> Right $ (Apply e1 e2, rest2)
          x -> x
      x -> x
  else if t == "run" then
    case parseToken ts of
      Right (e, rest) ->
        Right $ (Run e, rest)
      x -> x
  else if t == "nil" then
    Right $ (Nil, ts)
  else if t == "galaxy" then
    Right $ (CodeRef t, ts)
  else if isNumber t then
    Right $ (Number ((read t) :: Integer), ts)
  else if isCodeRef t then
    Right $ (CodeRef t, ts)
  else
    case stdFuncs Map.!? t of
      Just (f, a) -> Right $ (Func f a, ts)
      Nothing -> Left ("Unknown standard function "++t)

executeBinaryMath :: AlienCode -> AlienCode -> Map.Map String AlienCode -> (Integer -> Integer -> Integer) -> String -> Either String AlienCode
executeBinaryMath n1 n2 codeDict mathFunc funcName =
  case evaluate n1 codeDict of
    Right (Number n1) -> case evaluate n2 codeDict of
      Right (Number n2) -> Right $ Number (mathFunc n1 n2)
      Right x -> Left ("Right side of "++funcName++" is still " ++ (show x))
      x -> x
    Right x -> Left ("Left side of "++funcName++" is still " ++ (show x))
    x -> x

executeUnaryMath :: AlienCode -> Map.Map String AlienCode -> (Integer -> Integer) -> String -> Either String AlienCode
executeUnaryMath n1 codeDict mathFunc funcName =
  case evaluate n1 codeDict of
    Right (Number n1) -> Right $ Number (mathFunc n1)
    Right x -> Left ("Param of "++funcName++" is still " ++ (show x))
    x -> x

executeConditional :: AlienCode -> AlienCode -> Map.Map String AlienCode -> (Integer -> Integer -> Bool) -> String -> Either String AlienCode
executeConditional n1 n2 codeDict comparator funcName =
  case evaluate n1 codeDict of
    Right (Number n1) -> case evaluate n2 codeDict of
      Right (Number n2) -> if comparator n1 n2 then Right $ Func T 2 else Right $ Func F 2
      Right x -> Left ("Right side of "++funcName++" is still " ++ (show x))
      x -> x
    Right x -> Left ("Left side of "++funcName++" is still " ++ (show x))
    x -> x

runFunc :: AlienFunction -> [AlienCode] -> Map.Map String AlienCode -> Either String AlienCode
runFunc ADD (n1:n2:[]) codeDict =
  executeBinaryMath n1 n2 codeDict (+) "add"
runFunc MUL (n1:n2:[]) codeDict =
  executeBinaryMath n1 n2 codeDict (*) "mul"
runFunc DIV (n1:n2:[]) codeDict =
  executeBinaryMath n1 n2 codeDict rem "rem"

runFunc INC (n1:[]) codeDict =
  executeUnaryMath n1 codeDict (1 +) "inc"
runFunc DEC (n1:[]) codeDict =
  executeUnaryMath n1 codeDict (\n -> n - 1) "dec"
runFunc NEG (n1:[]) codeDict =
  executeUnaryMath n1 codeDict (\n -> -n) "neg"
runFunc POWER2 (n1:[]) codeDict =
  executeUnaryMath n1 codeDict (2 ^) "pwr2"


runFunc EQUALS (n1:n2:[]) codeDict = executeConditional n1 n2 codeDict (==) "eq"
runFunc LESSTHAN (n1:n2:[]) codeDict = executeConditional n1 n2 codeDict (<) "lt"

runFunc MOD (x:[]) codeDict =
  case evaluate x codeDict of
    Right (Number n) -> Right $ modulate (Number n)
    Right (Cons c1 c2) -> Right $ modulate (Cons c1 c2)
    Right x -> Left ("Can't modulate " ++ (show x))
    x -> x

runFunc DEM (bits:[]) codeDict =
  case evaluate bits codeDict of
    Right (BitString bits) -> Right $ demodulate bits
    Right x -> Left ("Can't demodulate "++(show x))
    x -> x

runFunc S (x0:x1:x2:[]) codeDict = doApply (Apply x0 x2) (Apply x1 x2) codeDict
runFunc C (x0:x1:x2:[]) codeDict = doApply (Apply x0 x1) x1 codeDict
runFunc B (x0:x1:x2:[]) codeDict = doApply x0 (Apply x1 x2) codeDict

runFunc T (x1:x2:[]) codeDict = Right $ x1
runFunc F (x1:x2:[]) codeDict = Right $ x2
runFunc I (x:[]) codeDict = Right $ x
runFunc CAR (c:[]) codeDict =
  case evaluate c codeDict of
    Right (Cons c1 c2) -> Right c1
    Right x -> Left ("Cannot take car of "++(show x))
    x -> x
runFunc CDR (c:[]) codeDict =
  case evaluate c codeDict of
    Right (Cons c1 c2) -> Right c2
    Right x -> Left ("Cannot take cdr of "++(show x))
    x -> x
runFunc ISNIL (x:[]) codeDict =
  case evaluate x codeDict of
    Right Nil -> Right $ Func T 2
    Right x -> Right $ Func F 2
    x -> x
runFunc IFZERO (n1:x:y:[]) codeDict =
  case evaluate n1 codeDict of
    Right (Number n1) ->
      if n1 == 0 then Right x else Right y
    Right x -> Left ("Cannot do if0 on "++(show x))
    x -> x

runFunc CONS (x0:x1:[]) codeDict = Right $ Cons x0 x1

runFunc SEND _ _ = Left "SEND not implemented"
runFunc DRAW _ _ = Left "DRAW not implemented"
runFunc CHECKERBOARD _ _ = Left "CHECKERBOARD not implemented"
runFunc MULTIPLEDRAW _ _ = Left "MULTIPLEDRAW not implemented"

runFunc f arglist _ = Left ("Can't execute function "++(show f)++" with args"++(unwords $ map show arglist))

-- Determine how many 4-bit units we need to encode the given integer.
numberSize :: Integer -> Integer
numberSize 0 = 0
numberSize x = if x < 16 then 1 else (numberSize (x `div` 16) + 1)

encodeUnary :: Integer -> [Int]
encodeUnary 0 = [0]
encodeUnary n = 1 : encodeUnary (n - 1)

encodeBits :: Integer -> Integer -> [Int]
encodeBits 0 _ = []
encodeBits nbits val = encodeBits (nbits - 1) (fromInteger (val `div` 2)) ++ [fromInteger (val `rem` 2)]

encodeInt :: Integer -> [Int]
encodeInt x = let size = numberSize x in encodeUnary size ++ encodeBits (4 * size) x

modulate (Number n1) = BitString (encodeInt n1)
modulate (Cons c1 c2) =
  let (BitString b1) = modulate c1 in
  let (BitString b2) = modulate c2 in
    BitString (b1++b2)
modulate Nil = BitString [0,0]

demodulate bs =
  fst $ demod bs

demod (0:0:tail) = (Nil,tail)
demod (0:1:tail) =
  let (v,xs) = decodeInt tail in (Number v,xs)
demod (1:0:tail) =
  let (v,xs) = decodeInt tail in (Number (-v),xs)
demod (1:1:tail) =
  let (x,tail1) = demod tail in
  let (y,tail2) = demod tail1 in
  (Cons x y, tail2)

-- Demodulate a string of 1s followed by a 0.
-- Returns the number of ones, and the remainder of the input list.
decodeUnary (0 : s) = (0, s)
decodeUnary (1 : s) =
  let (n, s') = decodeUnary s in
    (n + 1, s')

-- Demodulate the body of an integer.
-- The integer is big-endian.
decodeIntBits acc 0 s = (acc, s)
decodeIntBits acc n (0 : s) = decodeIntBits (2 * acc) (n - 1) s
decodeIntBits acc n (1 : s) = decodeIntBits (2 * acc + 1) (n - 1) s

-- Demodulate an integer.
decodeInt :: [Int] -> (Integer, [Int])
decodeInt s =
  let (len, s') = decodeUnary s in
    decodeIntBits 0 (4 * len) s'


doApply :: AlienCode -> AlienCode -> Map.Map String AlienCode -> Either String AlienCode
doApply x ac codeDict =
  case evaluate x codeDict of
    Right (Func f a) ->
      if a == 1 then
        runFunc f [ac] codeDict
      else
        Right $ PartialFunc f a [ac]
    Right (PartialFunc f a c) ->
      if a == 1 + length c then
        runFunc f (reverse (ac : c)) codeDict
      else
        Right $ PartialFunc f a (ac:c)
    Right x -> Left ("Unable to apply "++(show x))
    x -> x

evaluate :: AlienCode -> Map.Map String AlienCode -> Either String AlienCode
evaluate (Apply a1 a2) codeDict =
  trace ("Apply "++(show a1)++" -> "++(show a2)) (
  doApply a1 a2 codeDict
    )
evaluate (CodeRef r) codeDict =
  trace ("CodeRef "++r) (
  case codeDict Map.!? r of
    Just v -> evaluate v codeDict
    Nothing -> Left ("Unknown code reference " ++ r)
  )
evaluate (Run r) codeDict =
  trace ("Run "++ (show r)) (
  case evaluate r codeDict of
    Right r1 -> evaluate r1 codeDict
    x -> x
  )
evaluate x _ =
  trace ("Evaluate "++(show x)) (
  Right x
  )

repl dict = do
  putStr ">"
  hFlush stdout
  line <- getLine
  case parseToken (words line) of
    Right (token,_) ->
      let res = evaluate token dict in
      case res of
        Left x -> putStrLn x
        Right y -> putStrLn (show y)
    Left x -> putStrLn x
  repl dict

parseLineWithFail l =
  case parseLine l of
    Left x -> error x
    Right y -> y

main :: IO ()
main = do
    f <- readFile "../data/galaxy.txt"
    let l = lines f
    let dict = Map.fromList (map parseLineWithFail l)
    repl dict
