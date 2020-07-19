module Defender where

import DataTypes
import GameLogic

getDefenderCommands :: GameState -> [Command]
getDefenderCommands (GameState _ _ shipsAndCommands) =
  let enemies = getShipData AttackerRole shipsAndCommands in
  let myShips = getShipData DefenderRole shipsAndCommands in

  map (getDefenderCommand enemies) myShips

getDefenderCommand :: [ShipData] -> ShipData -> Command
getDefenderCommand enemies (ShipData shipId (myX,myY) myVel) =
  let (ShipData _ (enemyX,enemyY) _) = findClosest (myX,myY) enemies in
  let enemyDist = distanceSquared (myX,myY) (enemyX,enemyY) in
  if enemyDist < 200 then
    ShootCommand shipId (enemyX,enemyY) (IntValue 0)
  else
    let myAccelX = if enemyX > myX then -1
                   else if enemyX < myX then 1 else 0 in
    let myAccelY = if enemyY > myY then -1
                   else if enemyY < myY then 1 else 0 in
    AccelerateCommand shipId (myAccelX,myAccelY)
