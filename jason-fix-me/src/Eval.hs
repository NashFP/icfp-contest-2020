module Eval(Value(..), ap, stdlib) where

data Value =
    IntValue Integer
  | FunValue (Value -> Value)
  | NilValue
  | ConsValue Value Value

instance Show Value where
  show (IntValue i) = show i
  show NilValue = "nil"
  show (ConsValue h t) = "(" ++ show h ++ " : " ++ show t ++ ")"
  show (FunValue f) = "function () { [native code] }"

class ToValue t where
  toValue :: t -> Value

instance ToValue Value where
  toValue = id

instance ToValue Integer where
  toValue = IntValue

instance ToValue Bool where
  toValue True = t
  toValue False = f

ap (FunValue f) v = f v
ap (ConsValue a b) f = ap (ap f a) b
ap NilValue x = t

incImpl :: Value -> Value
incImpl (IntValue i) = IntValue (i + 1)
incImpl other = error $ "inc: TypeError: number expected, not " ++ show other

decImpl :: Value -> Value
decImpl (IntValue i) = IntValue (i - 1)
decImpl other = error $ "dec: TypeError: number expected, not " ++ show other

negImpl :: Value -> Value
negImpl (IntValue i) = IntValue (-i)
negImpl other = error $ "neg: TypeError: undefined is not a function: " ++ show other

binaryMathFunction :: ToValue t => String -> (Integer -> Integer -> t) -> Value
binaryMathFunction name f = FunValue impl
  where
    impl :: Value -> Value
    impl (IntValue i) = FunValue (impl2 i)
    impl other = error $ name ++ ": TypeError: first argument: number expected, not " ++ show other

    impl2 :: Integer -> Value -> Value
    impl2 i (IntValue j) = toValue (f i j)
    impl2 _ other = error $ name ++ ": TypeError: second argument: number expected, not " ++ show other


-- s f g x = f x (g x)
s = FunValue (\f -> FunValue (\g -> FunValue (\x -> ap (ap f x) (ap g x))))

-- b f g x = f (g x)
b = FunValue (\f -> FunValue (\g -> FunValue (\x -> ap f (ap g x))))

-- i x = x
i = FunValue id

-- t x y = x
t = FunValue (\x -> FunValue (\y -> x))

-- f x y = y
f = FunValue (\x -> FunValue (\y -> y))

-- ap ap ap c x0 x1 x2   =   ap ap x0 x2 x1
-- (((c x0) x1) x2)   =   ((x0 x2) x1)
-- c x0 x1 x2   =   x0 x2 x1
-- c f x y = f y x
c = FunValue (\f -> FunValue (\x -> FunValue (\y -> ap (ap f y) x)))

-- cons a b f = f a b
cons = FunValue (\a -> FunValue (\b -> ConsValue a b))

-- car x = x t
car = FunValue (\x -> ap x t)

-- cdr x = x f
cdr = FunValue (\x -> ap x f)

isnil = FunValue (\x -> case x of
                          NilValue -> t
                          _ -> f)

if0 = FunValue (\test -> FunValue (\a -> FunValue (\b -> if0Impl test a b)))
if0Impl (IntValue 0) a _ = a
if0Impl (IntValue _) _ b = b
if0Impl other _ _ = error $ "if0: TypeError: number expected, not " ++ show other

stdlib :: [(String, Value)]
stdlib = [
  ("s", s),
  ("b", b),
  ("i", i),
  ("t", t),
  ("f", f),
  ("c", c),
  ("nil", NilValue),
  ("cons", cons),
  ("car", car),
  ("cdr", cdr),
  ("neg", FunValue negImpl),
  ("lt", binaryMathFunction "lt" (<)),
  ("eq", binaryMathFunction "lt" (==)),
  ("inc", FunValue incImpl),
  ("dec", FunValue decImpl),
  ("add", binaryMathFunction "add" (+)),
  ("mul", binaryMathFunction "mul" (*)),
  ("div", binaryMathFunction "div" div),
  ("isnil", isnil)]
