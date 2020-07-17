module Main where

import Data.Char (digitToInt)
import Decode

main :: IO ()
main = do
  putStrLn ":) >"
  line <- getLine
  putStrLn $ show $ decode $ map digitToInt $ line
