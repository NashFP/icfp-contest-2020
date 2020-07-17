module Decode where

data Value = 
    Num Integer 
  | Cons Value Value
  | Nil 
  deriving Show

negateVal :: Value -> Value
negateVal (Num x) = Num (-x)
negateVal other = other

-- Demodulate a string of 1s followed by a 0.
-- Returns the number of ones, and the remainder of the input list.
decodeUnary :: [Int] -> (Integer, [Int])
decodeUnary (0 : s) = (0, s)
decodeUnary (1 : s) =
  let (n, s') = decodeUnary s in
    (n + 1, s')
decodeUnary x = error $ "Pattern match failed on " <> show x <> "!"

-- Demodulate the body of an integer.
-- The integer is big-endian.
-- decodeIntBits :: [Int] -> Int -> Int -> (Int, [Int])
decodeIntBits acc 0 s = (acc, s)
decodeIntBits acc n (0 : s) = decodeIntBits (2 * acc) (n - 1) s
decodeIntBits acc n (1 : s) = decodeIntBits (2 * acc + 1) (n - 1) s

-- Demodulate an integer.
decodeInt :: [Int] -> (Integer, [Int])
decodeInt s =
  let (len, s') = decodeUnary s in
    decodeIntBits 0 (4 * len) s'

-- Decode one value.
-- Returns the value, and the remainder of the input list.
decode :: [Int] -> (Value, [Int])
decode (0 : 0 : tail) = (Nil, tail)
decode (0 : 1 : tail) =
  let (v, xs) = decodeInt tail in (Num v, xs)
decode (1 : 0 : tail) =
  let (v, xs) = decodeInt tail in (Num (-v), xs)
decode (1 : 1 : tail) =
  let (x, tail') = decode tail in
    let (y, tail'') = decode tail' in
      (Cons x y, tail'')
