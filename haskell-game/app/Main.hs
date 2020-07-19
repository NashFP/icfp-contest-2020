module Main where

import System.Environment
import DataTypes
import Modulation
import Comm
import GameDriver

main :: IO ()
main = do
  args <- getArgs
  let url = args !! 0
  let playerKey = args !! 1
  if playerKey == "create" then
    doCreate url playerKey
  else
    startGame url playerKey

