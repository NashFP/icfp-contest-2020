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
  let urlExtra = if length args > 2 then
                   "?apiKey="++(args !! 2)
                 else
                   ""
  let fullUrl = url ++ "/aliens/send" ++ urlExtra

  if playerKey == "create" then
    doCreate fullUrl playerKey
  else
    startGame fullUrl playerKey

