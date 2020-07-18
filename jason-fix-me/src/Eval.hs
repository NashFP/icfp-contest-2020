module Eval(Value(..), eval, evaluateProgram, stdlib, repl) where

import Parse
import qualified Data.Map as Map
import System.IO
import DataTypes
import Encode
import Decode

class ToValue t where
  toValue :: t -> Value

instance ToValue Value where
  toValue = id

instance ToValue Integer where
  toValue = IntValue

instance ToValue Bool where
  toValue True = t
  toValue False = f

-- The type of environments. An environment is basically a collection of
-- name-value pairs.
type Env = Map.Map String Value

-- Look up the value of a variable in an environment.
getValueOfVariable :: Env -> String -> Value
getValueOfVariable env key =
  case Map.lookup key env of
    Nothing -> error ("no such variable: " ++ key)
    Just value -> value

ap (FunValue _ f) v = f v
ap (ConsValue a b) f = ap (ap f a) b
ap NilValue x = t
ap other arg = error $ "ap: TypeError: function expected, not " ++ show other

-- The alien standard library

incImpl :: Value -> Value
incImpl (IntValue i) = IntValue (i + 1)
incImpl other = error $ "inc: TypeError: number expected, not " ++ show other

decImpl :: Value -> Value
decImpl (IntValue i) = IntValue (i - 1)
decImpl other = error $ "dec: TypeError: number expected, not " ++ show other

negImpl :: Value -> Value
negImpl (IntValue i) = IntValue (-i)
negImpl other = error $ "neg: TypeError: undefined is not a function: " ++ show other

modulate :: Value -> Value
modulate (IntValue i) = encode (IntValue i)
modulate (ConsValue c1 c2) = encode (ConsValue c1 c2)
modulate other = error $ "attempted to modulate " ++ show other

demodulate :: Value -> Value
demodulate (BitStringValue bits) = fst $ decode (BitStringValue bits)
demodulate other = error $ "attempted to demodulate "++show other

modem :: Value -> Value
modem x = modulate $ demodulate x

binaryMathFunction :: ToValue t => String -> (Integer -> Integer -> t) -> Value
binaryMathFunction name f = FunValue name impl
  where
    impl :: Value -> Value
    impl (IntValue i) = FunValue ("(" ++ name ++ " " ++ show i ++ ")") (impl2 i)
    impl other = error $ name ++ ": TypeError: first argument: number expected, not " ++ show other

    impl2 :: Integer -> Value -> Value
    impl2 i (IntValue j) = toValue (f i j)
    impl2 _ other = error $ name ++ ": TypeError: second argument: number expected, not " ++ show other


-- s f g x = f x (g x)
s = FunValue "s" (\f -> FunValue "(s _)" (\g -> FunValue "(s _ _)" (\x -> ap (ap f x) (ap g x))))

-- b f g x = f (g x)
b = FunValue "b" (\f -> FunValue "(b _)" (\g -> FunValue "(b _ _)" (\x -> ap f (ap g x))))

-- i x = x
i = FunValue "i" id

-- t x y = x
t = FunValue "t" (\x -> FunValue ("(t " ++ show x ++ ")") (\y -> x))

-- f x y = y
f = FunValue "f" (\x -> i)

-- c f x y = f y x
c = FunValue "c" (\f -> FunValue "(c _)" (\x -> FunValue "(c _ _)" (\y -> ap (ap f y) x)))

-- cons a b f = f a b
cons = FunValue "cons" (\a -> FunValue ("(cons " ++ show a ++ ")") (\b -> ConsValue a b))

-- car x = x t
car = FunValue "car" (\x -> ap x t)

-- cdr x = x f
cdr = FunValue "cdr" (\x -> ap x f)

isnil = FunValue "isnil" (\x -> case x of
                                  NilValue -> t
                                  _ -> f)

if0 = FunValue "if0" if0Impl
if0Impl (IntValue 0) = t
if0Impl (IntValue _) = f
if0Impl other = error $ "if0: TypeError: number expected, not " ++ show other

plot = FunValue "plot" (\v -> BitmapValue $ render $ fmap valueToCoordinatePair $ valueToList v)

valueToList NilValue = []
valueToList (ConsValue h t) = h : valueToList t
valueToList other = error $ "valueToList: TypeError: list expected, not " ++ show other

valueToCoordinatePair (ConsValue (IntValue x) (IntValue y)) = (x, y)

render :: [(Integer, Integer)] -> [String]
render pairs =
  let minX = minimum $ fmap fst pairs
      maxX = maximum $ fmap fst pairs
      minY = minimum $ fmap snd pairs
      maxY = maximum $ fmap snd pairs
  in [[if elem (x, y) pairs then '*' else '.'
         | x <- [minX..maxX]]
       | y <- [minY..maxY]]


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
  ("neg", FunValue "neg" negImpl),
  ("lt", binaryMathFunction "lt" (<)),
  ("eq", binaryMathFunction "lt" (==)),
  ("inc", FunValue "inc" incImpl),
  ("dec", FunValue "inc" decImpl),
  ("add", binaryMathFunction "add" (+)),
  ("mul", binaryMathFunction "mul" (*)),
  ("div", binaryMathFunction "div" div),
  ("isnil", isnil),
  ("plot", plot),
  ("mod", FunValue "mod" modulate),
  ("dem", FunValue "dem" demodulate),
  ("modem", FunValue "modem" modem),
  ("if0", if0)]


-- Evaluate an expression (lazily).
eval :: Env -> Expression -> Value
eval env (Constant i) = IntValue i
eval env (Identifier name) = getValueOfVariable env name
eval env (Apply f arg) = ap (eval env f) (eval env arg)
eval env (BitString bits) = BitStringValue bits

-- Evaluate a program, returning the populated environment.
evaluateProgram :: [(String, Expression)] -> Env
evaluateProgram parsedEquations =
  let env :: Env
      env = Map.fromList (stdlib ++ [(name, eval env expr) | (name, expr) <- parsedEquations])
  in env

-- Run a read-eval-print loop.
repl :: Env -> IO ()
repl env = do
  putStr "\x1b[36m\x1b[1m*>\x1b[0m "
  hFlush stdout
  line <- getLine
  case parse replEntry "<stdin>" line of
    Left errors -> putStrLn $ "syntax error: " ++ show errors
    Right value -> putStrLn $ show $ eval env value
  repl env