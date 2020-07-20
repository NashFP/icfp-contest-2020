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
    let xAccel' = if xAccel == 0 then
                    if (myX < 0) && (myXVel > 0) then 1 
                    else if (myX > 0) && (myXVel < 0) 
                         then -1 else 0
                    else xAccel in
    let yAccel' = if yAccel == 0 then
                    if (myY < 0) && (myYVel > 0) then 1 
                    else if (myY > 0) && (myYVel < 0) 
                         then -1 else 0
                    else yAccel in
    let cmds = [AccelerateCommand shipId $ (xAccel',yAccel')] in
    let myPredX = myX + myXVel in
    let myPredY = myY + myYVel in


    let (ShipData _ (enemyX,enemyY) (enemyXVel,enemyYVel)) = findClosest (myX,myY) enemies in
    let enemyPredX = enemyX + enemyXVel in
    let enemyPredY = enemyY + enemyYVel in
    if length enemies == 1 && abs (myPredX-enemyPredX) <= 6 && abs (myPredY-enemyPredY) <= 6 then
      (DetonateCommand shipId) : cmds
    else if ticks `rem` 3 == 0 then
      (ShootCommand shipId (enemyX+enemyXVel,enemyY+enemyYVel) 48) : cmds
    else
      cmds


