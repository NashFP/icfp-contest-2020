module Encode (encode) where

import DataTypes

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

encode' :: Value -> [Int]
encode' NilValue = [0, 0]
encode' (IntValue x) =
  if x >= 0 then [0, 1] ++ encodeInt x
  else [1, 0] ++ encodeInt (-x)
encode' (ConsValue a b) = [1, 1] ++ encode' a ++ encode' b

encode :: Value -> Value
encode v = BitStringValue $ encode' v
