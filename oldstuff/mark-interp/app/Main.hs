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
  ("t", (T,3)),
  ("f", (F,3)),
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

runFunc :: AlienFunction -> [AlienCode] -> Either String AlienCode
runFunc ADD (Number n1:Number n2:[]) = Right $ Number (n1+n2)
runFunc MUL (Number n1:Number n2:[]) = Right $ Number (n1*n2)
runFunc DIV (Number n1:Number n2:[]) = Right $ Number (n1 `rem` n2)
runFunc DEC (Number n1:[]) = Right $ Number (n1-1)
runFunc INC (Number n1:[]) = Right $ Number (n1+1)
runFunc NEG (Number n1:[]) = Right $ Number (-n1)
runFunc POWER2 (Number n1:[]) = Right $ Number (2 ^ n1)
runFunc EQUALS (Number n1:Number n2:[]) = if n1 == n2 then Right $ (Func T 3) else Right $ (Func F 3)
runFunc LESSTHAN (Number n1:Number n2:[]) = if n1 < n2 then Right $ (Func T 3) else Right $ (Func F 3)
runFunc MOD (x:[]) = Right $ modulate x
runFunc DEM (BitString bits:[]) = Right $ demodulate bits
runFunc S (x0:x1:x2:[]) = Right $ Apply (Apply x0 x2) (Apply x1 x2)
runFunc C (x0:x1:x2:[]) = Right $ Apply (Apply x0 x1) x1
runFunc B (x0:x1:x2:[]) = Right $ Apply x0 (Apply x1 x2)
runFunc T (Func T _:x1:x2:[]) = Right $ x1
runFunc F (Func F _:x1:x2:[]) = Right $ x2
runFunc I (x:[]) = Right $ x
runFunc CAR (Cons c1 c2:[]) = Right $ c1
runFunc CDR (Cons c1 c2:[]) = Right $ c2
runFunc ISNIL (Nil:[]) = Right $ Func T 3
runFunc ISNIL _ = Right $ Func F 3
runFunc IFZERO (Number n1:x:y:[]) = if n1 == 0 then Right $ x else Right $ y
runFunc CONS (x0:x1:[]) = Right $ Cons x0 x1

runFunc SEND _ = Left "SEND not implemented"
runFunc DRAW _ = Left "DRAW not implemented"
runFunc CHECKERBOARD _ = Left "CHECKERBOARD not implemented"
runFunc MULTIPLEDRAW _ = Left "MULTIPLEDRAW not implemented"

runFunc f arglist = Left ("Can't execute function "++(show f)++" with args"++(unwords $ map show arglist))

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
doApply (Func f a) (CodeRef r) codeDict =
  case evaluate (CodeRef r) codeDict of
    Right acEval ->
      trace ("Expanded CodeRef to "++(show acEval)) (
      if a == 1 then
        runFunc f [acEval]
      else
        Right $ PartialFunc f a [acEval]
        )
    x -> x
doApply (PartialFunc f a c) (CodeRef r) codeDict=
  case evaluate (CodeRef r) codeDict of
    Right acEval ->
      trace ("Expanded CodeRef to "++(show acEval)) (
      if a == 1 + length c then
        runFunc f (reverse (acEval : c))
      else
        Right $ PartialFunc f a (acEval:c)
        )
doApply (Func f a) ac codeDict =
  if a == 1 then
    runFunc f [ac]
  else
    Right $ PartialFunc f a [ac]
doApply (PartialFunc f a c) ac codeDict=
  if a == 1 + length c then
    runFunc f (reverse (ac : c))
  else
    Right $ PartialFunc f a (ac:c)

doApply ac1 ac2 _ =
  Left ("Invalid arguments to apply " ++ (show ac1) ++ " -> " ++ (show ac2))

evaluate :: AlienCode -> Map.Map String AlienCode -> Either String AlienCode
evaluate (Apply a1 a2) codeDict =
  trace ("Apply "++(show a1)++" -> "++(show a2)) (
  case evaluate a1 codeDict of
    Right app1 ->
      case evaluate a2 codeDict of
        Right app2 -> doApply app1 app2 codeDict
        x -> x
    x -> x
    )
evaluate (CodeRef r) codeDict =
  trace ("CodeRef "++r) (
  case codeDict Map.!? r of
    Just v -> Right v
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
