module Defender where

import DataTypes
import GameLogic

getDefenderCommands :: GameState -> [Command]
getDefenderCommands (GameState ticks _ shipsAndCommands) =
  let enemies = getShipData AttackerRole shipsAndCommands in
  let myShips = getShipData DefenderRole shipsAndCommands in

  concatMap (getDefenderCommand ticks (length myShips) enemies) myShips

getDefenderCommand :: Integer -> Int -> [ShipData] -> ShipData -> [Command]
getDefenderCommand ticks numShips enemies (ShipData shipId (myX,myY) (myXVel,myYVel) energy) =
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
    let accelCommand = [AccelerateCommand shipId $ (xAccel',yAccel')] in
    let cmds = if shipId < 2 && ticks > 20 && numShips < 15 then
                 (SpawnCommand shipId [0,0,0,1]) : accelCommand
               else
                 accelCommand
               in
    cmds
--    let (ShipData _ (enemyX,enemyY) _) = findClosest (myX,myY) enemies in
--    if ticks `rem` 5 == 0 then
--      (ShootCommand shipId (enemyX,enemyY) (IntValue 0)) : cmds
--    else
--      cmds


