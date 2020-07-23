module DataTypes where

import Data.List (intercalate)

data Value =
    IntValue Integer
  | FunValue String (Value -> Value)
  | NilValue
  | ConsValue Value Value
  | BitmapValue [String]
  | BitStringValue [Int]

showAsListTail :: Value -> String
showAsListTail (ConsValue h t) = " " ++ show h ++ showAsListTail t
showAsListTail NilValue = ""
showAsListTail other = " . " ++ show other

instance Show Value where
  show (IntValue i) = show i
  show NilValue = "nil"
  show (ConsValue h t) = "[" ++ show h ++ showAsListTail t ++ "]"
  show (FunValue name f) = name
  show (BitmapValue lines) = concat $ map (++ "\n") lines
  show (BitStringValue bits) = '%' : concat (map show bits)

instance Eq Value where
  (==) (IntValue x) (IntValue y) = x == y
  (==) NilValue NilValue = True
  (==) (ConsValue h1 t1) (ConsValue h2 t2) = (h1, t1) == (h2, t2)
  (==) (FunValue name1 f1) (FunValue name2 f2) = name1 == name2
  (==) (BitmapValue lines1) (BitmapValue lines2) = lines1 == lines2
  (==) (BitStringValue bits1) (BitStringValue bits2) = bits1 == bits2
  (==) _ _ = False

