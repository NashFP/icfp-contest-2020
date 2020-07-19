module Decode  (decode) where

import DataTypes

-- Flip the sign of an integer. Leaves other values unchanged.
negateVal (IntValue x) = IntValue (-x)
negateVal other = other


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

-- Decode one value.
-- Returns the value, and the remainder of the input list.
decode :: Value -> (Value, [Int])
decode (BitStringValue (0 : 0 : tail)) = (NilValue, tail)
decode (BitStringValue (0 : 1 : tail)) =
  let (v, xs) = decodeInt tail in (IntValue v, xs)
decode (BitStringValue (1 : 0 : tail)) =
  let (v, xs) = decodeInt tail in (IntValue (-v), xs)
decode (BitStringValue (1 : 1 : tail)) =
  let (x, tail') = decode (BitStringValue tail) in
    let (y, tail'') = decode (BitStringValue tail') in
      (ConsValue x y, tail'')
