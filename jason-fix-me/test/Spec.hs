import Test.HUnit
import qualified Data.Map as Map
import Eval(Value(..), eval, stdlib)
import Parse

main :: IO Counts
main = runTestTT $ TestList 
    [ assertEvalEqual "1" $ IntValue 1
    , assertEvalEqual "-1" $ IntValue $ -1
    , assertEvalEqual "ap inc 0" $ IntValue 1
    , assertEvalEqual "ap dec 0" $ IntValue $ -1
    , assertEvalEqual "ap ap add 1 2" $ IntValue 3
    , assertEvalEqual "ap ap mul 2 3" $ IntValue 6
    , assertEvalEqual "ap ap div 7 2" $ IntValue 3
    , assertEvalEqual "ap ap eq 0 0" $ FunValue "t" id
    , assertEvalEqual "ap ap eq 0 1" $ FunValue "f" id
    , assertEvalEqual "ap ap lt 0 1" $ FunValue "t" id
    , assertEvalEqual "ap ap lt 2 1" $ FunValue "f" id
    , assertEvalEqual "ap neg 1" $ IntValue $ -1
    , assertEvalEqual "ap inc ap inc 0" $ IntValue 2
    , assertEvalEqual "ap ap ap s mul ap add 1 6" $ IntValue 42
    , assertEvalEqual "ap ap ap c add 1 2" $ IntValue 3
    , assertEvalEqual "ap ap ap b inc dec 5" $ IntValue 5
    , assertEvalEqual "ap ap t 1 5" $ IntValue 1
    , assertEvalEqual "ap ap f 1 5" $ IntValue 5
    , assertEvalEqual "ap ap cons 1 2" $ ConsValue (IntValue 1) (IntValue 2)
    , assertEvalEqual "ap car ap ap cons 0 1" $ IntValue 0
    , assertEvalEqual "ap cdr ap ap cons 0 1" $ IntValue 1
    , assertEvalEqual "ap nil 1" $ FunValue "t" id
    , assertEvalEqual "ap isnil nil" $ FunValue "t" id
    , assertEvalEqual "ap isnil ap ap cons 0 1" $ FunValue "f" id
    , assertEvalEqual "ap mod 0" $ BitStringValue [0, 1, 0]
    , assertEvalEqual "ap mod 1" $ BitStringValue [0, 1, 1, 0, 0, 0, 0, 1]
    , assertEvalEqual "ap mod -256" $ BitStringValue [1,0,1,1,1,0,0,0,0,1,0,0,0,0,0,0,0,0]
    , assertEvalEqual "ap dem %010" $ IntValue 0
    ]

assertEvalEqual :: String -> Value -> Test
assertEvalEqual expression expected =
    TestLabel expression $
        case evalLine expression of
            Left errors -> 
                TestCase $ assertFailure errors
            Right actual ->
                TestCase $ assertEqual ("= " ++ (show expected)) expected actual

evalLine :: String -> Either String Value
evalLine line =
    case parse replEntry "<stdin>" line of
        Left errors -> Left $ "syntax error: " ++ show errors
        Right value -> Right $ eval initEnv value

initEnv = Map.fromList stdlib