{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
-- import Network.HTTP.Simple
-- import Data.ByteString.Lazy.UTF8 as BLU
-- import Control.Exception

-- import Encode
-- import Decode
import Parse
import Eval
import qualified Lambda

main = do
  args <- getArgs
  program <- parseFile (args!!0)
  putStrLn $ concat $ fmap Lambda.prettyPrintEquation $ program
  let env = evaluateProgram program
  repl env

-- stack run "https://icfpc2020-api.testkontur.ru/aliens/send?apiKey=$API_KEY" "1101000"
-- main = catch (
--     do  
--         args <- getArgs
--         putStrLn ("ServerUrl: " ++ args!!0 ++ "; PlayerKey: " ++ args!!1)
--         request' <- parseRequest ("POST " ++ (args!!0))
--         let request = setRequestBodyLBS (BLU.fromString (args!!1)) request'
--         response <- httpLBS request
--         let statuscode = show (getResponseStatusCode response)
--         case statuscode of
--             "200" -> putStrLn ("Server response: " ++ BLU.toString (getResponseBody response))
--             _ -> putStrLn ("Unexpected server response:\nHTTP code: " ++ statuscode ++ "\nResponse body: " ++ BLU.toString (getResponseBody response))
--     ) handler
--     where
--         handler :: SomeException -> IO ()
--         handler ex = putStrLn $ "Unexpected server response:\n" ++ show ex



--main :: IO ()
--main = putStrLn $ map intToDigit $ encode (Cons (Num 0) Nil)


-- main = do
--   putStrLn ":) >"
--   line <- getLine
--   putStrLn $ show $ decode $ map digitToInt $ line
-- 
