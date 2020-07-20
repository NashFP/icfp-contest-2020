module Modulation where

import DataTypes

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
decodeInt :: [Int] -> (Integer, [Int])
decodeInt s =
  let (len, s') = decodeUnary s in
    decodeIntBits 0 (4 * len) s'

decode :: [Int] -> (AlienData,[Int])
decode (0:0:tail) = (ListValue [],tail)
decode (0:1:tail) =
  let (i, xs) = decodeInt tail in (IntValue i, xs)
decode (1:0:tail) =
  let (i, xs) = decodeInt tail in (IntValue (-i),xs)
decode (1:1:tail) =
  let (x,tail') = decode tail in
    let (y,tail'') = decode tail' in
      case y of
        IntValue i ->
          case x of
            IntValue x0 -> (PairValue (x0,i),tail'')
            ListValue x0 -> (ListValue [x,y],tail'')
        ListValue l -> (ListValue (x:l),tail'')

demodulate :: [Int] -> AlienData
demodulate l = fst $ decode l

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

modulate :: AlienData -> [Int]
modulate (ListValue []) = [0,0]
modulate (ListValue (x:xs)) =
  1:1:(modulate x)++(modulate $ ListValue xs)
modulate (IntValue x) =
  if x >= 0 then 0:1:encodeInt x
  else 1:0:encodeInt (-x)
modulate (PairValue (x,y)) =
  (1:1:modulate (IntValue x))++(modulate (IntValue y))

