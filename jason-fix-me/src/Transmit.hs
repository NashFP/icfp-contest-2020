module Transmit where

import System.Environment
import Network.HTTP.Simple
import Data.ByteString.Lazy.UTF8 as BLU
import Control.Exception

transmitWithArgs :: IO ()
transmitWithArgs = do
    args <- getArgs
    transmit (args !! 0) (args !! 1)

transmit :: String -> String -> IO ()
transmit serverUrl playerKey = handle handler $ do
    putStrLn $ "ServerUrl: " <> serverUrl <> "; PlayerKey: " <> playerKey
    request' <- parseRequest ("POST " <> serverUrl)
    let request = setRequestBodyLBS (BLU.fromString playerKey) request'
    response <- httpLBS request
    let statuscode = show (getResponseStatusCode response)
    case statuscode of
      "200" -> putStrLn $ "Server response: " 
               <> BLU.toString (getResponseBody response)

      _     -> putStrLn $ "Unexpected server response:"
               <> "\nHTTP code: " <> statuscode 
               <> "\nResponse body: " 
               <> BLU.toString (getResponseBody response)
  where
    handler :: SomeException -> IO ()
    handler ex = putStrLn $ "Unexpected server response:\n" ++ show ex