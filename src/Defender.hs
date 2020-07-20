module Defender where

import DataTypes
import GameLogic

getDefenderCommands :: GameState -> [Command]
getDefenderCommands (GameState _ _ shipsAndCommands) =
  let enemies = getShipData AttackerRole shipsAndCommands in
  let myShips = getShipData DefenderRole shipsAndCommands in

  concatMap (getDefenderCommand enemies) myShips

getDefenderCommand :: [ShipData] -> ShipData -> [Command]
getDefenderCommand enemies (ShipData shipId (myX,myY) (myXVel,myYVel)) =
--  let (ShipData _ (enemyX,enemyY) _) = findClosest (myX,myY) enemies in
--  let enemyDist = distanceSquared (myX,myY) (enemyX,enemyY) in
--  if enemyDist < 200 then
--    ShootCommand shipId (enemyX,enemyY) (IntValue 0)
--  else
    let (orbitX,orbitY) = computeOrbitVector (myX,myY) in
    let xAccel = if myXVel > 8 then 1 else if myXVel < -8 then -1 else orbitX in
    let yAccel = if myYVel > 8 then 1 else if myYVel < -8 then -1 else orbitY in
    [AccelerateCommand shipId $ (xAccel,yAccel)]
