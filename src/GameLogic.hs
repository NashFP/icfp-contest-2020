module GameLogic where

import DataTypes

enemyRole :: Role -> Role
enemyRole AttackerRole = DefenderRole
enemyRole DefenderRole = AttackerRole

data ShipData = ShipData Integer (Integer,Integer) (Integer,Integer) deriving (Eq,Show)

getShipsAndCommandsForRole :: Role -> [ShipAndCommands]-> [ShipAndCommands]
getShipsAndCommandsForRole role shipsAndCommands =
  filter (hasRole role) shipsAndCommands
    where
      hasRole wantRole (ShipAndCommands (Ship role _ _ _ _ _ _ _) _) = role == wantRole

getShipData :: Role -> [ShipAndCommands] -> [ShipData]
getShipData role shipsAndCommands =
  map getPosAndVelocity $ getShipsAndCommandsForRole role shipsAndCommands
    where
      getPosAndVelocity (ShipAndCommands (Ship _ shipId pos velocity _ _ _ _) _) = ShipData shipId pos velocity

distanceSquared :: (Integer,Integer) -> (Integer,Integer) -> Integer
distanceSquared (x1,y1) (x2,y2) =
  (x2-x1) * (x2-x1) + (y2-y1) * (y2-y1)

findClosest :: (Integer,Integer) -> [ShipData] -> ShipData
findClosest pos ((ShipData shipId otherPos vel):ss) =
  findClosest' pos (ShipData shipId otherPos vel) (distanceSquared pos otherPos) ss

findClosest' :: (Integer,Integer) -> ShipData -> Integer -> [ShipData] -> ShipData
findClosest' myPos currClosest currDistSquared [] = currClosest
findClosest' myPos currClosest currDistSquared ((ShipData shipId otherPos vel):ss) =
  let dist = distanceSquared myPos otherPos in
  if dist < currDistSquared then
    findClosest' myPos (ShipData shipId otherPos vel) dist ss
  else
    findClosest' myPos currClosest currDistSquared ss


computeAngle :: (Integer,Integer) -> Double
computeAngle (xi,yi) =
  let x = fromIntegral xi in
  let y = fromIntegral yi in
  if (xi == 0) && (yi == 0) then
    0.0
  else
    atan2 y x

computeOrbitVector :: (Integer,Integer) -> (Integer,Integer)
computeOrbitVector (xi,yi) =
  let angle = computeAngle (xi,yi) in
  let xdist = round (-sin angle) in
  let ydist = round (cos angle) in
  (xdist,ydist)
  