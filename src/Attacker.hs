module Attacker where

import DataTypes
import GameLogic

getAttackerCommands ::GameState -> [Command]
getAttackerCommands (GameState ticks _ shipsAndCommands) =
  let enemies = getShipData DefenderRole shipsAndCommands in
  let myShips = getShipData AttackerRole shipsAndCommands in
  concatMap (getAttackerCommand ticks enemies) myShips

getAttackerCommand :: Integer -> [ShipData] -> ShipData -> [Command]
getAttackerCommand ticks enemies (ShipData shipId (myX,myY) (myXVel,myYVel)) =
    let (orbitX,orbitY) = computeOrbitVector (myX,myY) in
    let xAccel = if myXVel > 5 then 1 else if myXVel < -5 then -1 else orbitX in
    let yAccel = if myYVel > 5 then 1 else if myYVel < -5 then -1 else orbitY in
    let cmds = [AccelerateCommand shipId $ (xAccel,yAccel)] in

    let (ShipData _ (enemyX,enemyY) _) = findClosest (myX,myY) enemies in
    let enemyDist = distanceSquared (myX,myY) (enemyX,enemyY) in
    if ticks `rem` 3 == 0 then
      (ShootCommand shipId (enemyX,enemyY) (IntValue 0)) : cmds
    else
      cmds


