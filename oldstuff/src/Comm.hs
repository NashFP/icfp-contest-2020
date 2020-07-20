module Comm where

import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Modulation
import DataTypes
import Data.Char (digitToInt,intToDigit)
import Control.Exception

sendToServer :: String -> String -> AlienData -> IO AlienData
sendToServer serverURL playerKey alienData =
  catch (
    do
      request' <- parseRequest ("POST "++serverURL)
      let modulatedData = modulate alienData
      let modulatedString = map intToDigit modulatedData
      putStrLn $ "Sending request "++show alienData
      putStrLn $ "Sending data "++modulatedString
      let request = setRequestBodyLBS (BLU.fromString modulatedString) request'
      response <- httpLBS request
      let statuscode = show (getResponseStatusCode response)
      case statuscode of
        "200" -> do
          let respStr = BLU.toString $ getResponseBody response
          putStrLn $ "Got reply "++respStr
          return $ demodulate $ map digitToInt respStr
        _ -> error ("Unexpected server response:\nHTTP code: " ++ statuscode ++ "\nResponse body: " ++ BLU.toString (getResponseBody response))
    ) handler
    where
      handler :: SomeException -> IO AlienData
      handler ex = error $ "Unexpected server response:\n" ++ show ex

