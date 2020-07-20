module Defender where

import DataTypes
import GameLogic

getDefenderCommands :: GameState -> [Command]
getDefenderCommands (GameState ticks _ shipsAndCommands) =
  let enemies = getShipData AttackerRole shipsAndCommands in
  let myShips = getShipData DefenderRole shipsAndCommands in

  concatMap (getDefenderCommand ticks enemies) myShips

getDefenderCommand :: Integer -> [ShipData] -> ShipData -> [Command]
getDefenderCommand ticks enemies (ShipData shipId (myX,myY) (myXVel,myYVel) energy) =
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

    cmds
--    let (ShipData _ (enemyX,enemyY) _) = findClosest (myX,myY) enemies in
--    if ticks `rem` 5 == 0 then
--      (ShootCommand shipId (enemyX,enemyY) (IntValue 0)) : cmds
--    else
--      cmds


