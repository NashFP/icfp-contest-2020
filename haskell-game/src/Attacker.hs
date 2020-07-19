module Attacker where

import DataTypes
import GameLogic

getAttackerCommands ::GameState -> [Command]
getAttackerCommands (GameState _ _ shipsAndCommands) =
  let enemies = getShipData DefenderRole shipsAndCommands in
  let myShips = getShipData AttackerRole shipsAndCommands in
  map (getAttackerCommand enemies) myShips

getAttackerCommand :: [ShipData] -> ShipData -> Command
getAttackerCommand enemies (ShipData shipId (myX,myY) myVel) =
  let (ShipData _ (enemyX,enemyY) _) = findClosest (myX,myY) enemies in
  let enemyDist = distanceSquared (myX,myY) (enemyX,enemyY) in
  if enemyDist < 200 then
    ShootCommand shipId (enemyX,enemyY) (IntValue 0)
  else
    let myAccelX = if enemyX < myX then -1
                   else if enemyX > myX then 1 else 0 in
    let myAccelY = if enemyY < myY then -1
                   else if enemyY > myY then 1 else 0 in
    AccelerateCommand shipId (myAccelX,myAccelY)


