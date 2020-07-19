module Eval(Value(..), eval, evaluateProgram, stdlib, repl) where

import Debug.Trace
import Parse
import qualified Data.Map as Map
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Char (digitToInt, intToDigit)
import Data.List (intercalate)
import DataTypes
import Encode
import Decode
import Control.Exception
import Network.HTTP.Simple

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
modem x = demodulate $ modulate x

sendToServer :: [Int] -> Value
sendToServer x =
  unsafePerformIO $ catch (
     do
         request' <- parseRequest ("POST " ++ "https://icfpc2020-api.testkontur.ru/aliens/send?apiKey=8d61d5ef5c3b48d880d5937a4dcd308d")
         let request = setRequestBodyLBS (BLU.fromString (map intToDigit x)) request'
         response <- httpLBS request
         let statuscode = show (getResponseStatusCode response)
         case statuscode of
             "200" -> return $ BitStringValue $ map digitToInt (BLU.toString $ getResponseBody response)
             _ -> error ("Unexpected server response:\nHTTP code: " ++ statuscode ++ "\nResponse body: " ++ BLU.toString (getResponseBody response))
     ) handler
     where
         handler :: SomeException -> IO Value
         handler ex = error $ "Unexpected server response:\n" ++ show ex

send :: Value -> Value
send x =
  let BitStringValue modx = encode x in
  let result = sendToServer modx in
  demodulate result

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

f38 :: Value
f38 = FunValue "f38" f38'

f38' :: Value -> Value
f38' v = FunValue ("(" ++ "f38" ++ " " ++ show v ++ ")") (f38'' v)

f38'' :: Value -> Value -> Value
f38'' protocol (ConsValue c1 c2) =
  let (IntValue flag:newState:d:[]) = valueToList (ConsValue c1 c2) in
    if flag == 0 then
    trace ("f38 flag = 0 with newState "++(show $ modulate newState)) (
      ConsValue (modem newState) (multipleDraw d)
      )
    else
      trace ("f38 flag = 1 with newState "++(show $ modulate newState)++" and d "++(show d)) (
      ap (ap (ap Eval.interact protocol) (modem newState)) (send d)
      )
f38'' _ other = error $ "f38" ++ ": TypeError: second argument: ConsValue expected, not " ++ show other

interact :: Value
interact = FunValue "interact" interact'

interact' :: Value -> Value
interact' v = FunValue ("(" ++ "interact" ++ " " ++ show v ++ ")") (interact'' v)

interact'' :: Value -> Value -> Value
interact'' v1 v2 = FunValue ("(" ++ "interact" ++ " " ++ show v1 ++ " "++ show v2 ++ ")") (interact''' v1 v2)

interact''' :: Value -> Value -> Value -> Value
interact''' protocol state vector = (ap (ap f38 protocol) (ap (ap protocol state) vector))

draw = FunValue "draw" (\v -> BitmapValue $ render $ fmap valueToCoordinatePair $ valueToList v)

writeBitmaps :: [Value] -> [Value]
writeBitmaps bitmaps =
  unsafePerformIO $ do
    appendFile "pics.txt" (intercalate "\n" (map show bitmaps))
    return bitmaps


renderBitmap NilValue = NilValue
renderBitmap v =
    let cp = fmap valueToCoordinatePair $ valueToList v in
      BitmapValue $ render cp

multipleDraw :: Value -> Value
multipleDraw v =
  let bitmaps = map renderBitmap $ valueToList v in
    listToValue $ writeBitmaps bitmaps

listToValue :: [Value] -> Value
listToValue [] = NilValue
listToValue (x:xs) = ConsValue x $ listToValue xs

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
  ("draw", draw),
  ("multipledraw", FunValue "multipledraw" multipleDraw),
  ("mod", FunValue "mod" modulate),
  ("dem", FunValue "dem" demodulate),
  ("modem", FunValue "modem" modem),
  ("send", FunValue "send" send),
  ("f38", f38),
  ("interact", Eval.interact),
  ("if0", if0)]


-- Evaluate an expression (lazily).
eval :: Env -> Expression -> Value
eval env (Constant i) = IntValue i
eval env (ListLiteral expressions) = consFromList env expressions
eval env (Identifier name) = getValueOfVariable env name
eval env (Apply f arg) = ap (eval env f) (eval env arg)
eval env (BitString bits) = BitStringValue bits

consFromList :: Env -> [Expression] -> Value
consFromList env expressions =
  let 
    values = map (eval env) expressions
    f current acc = ConsValue current acc
  in
    foldr f NilValue values

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
