{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import Network.HTTP.Simple
import Data.ByteString.Lazy.UTF8 as BLU
import Data.Map as Map
import Control.Exception

--import Data.Char
--import Encode
--import Decode
import Parse


prettyPrintEquation (name, value) = name ++ " = " ++ show value ++ "\n"


main = do
  args <- getArgs
  variables <- parseFile (args!!0)
  putStrLn $ concat $ fmap prettyPrintEquation $ Map.toList $ variables


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
