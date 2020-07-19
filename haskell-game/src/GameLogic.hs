module GameLogic where

import DataTypes

getShipsAndCommandsForRole :: Role -> [ShipAndCommands]-> [ShipAndCommands]
getShipsAndCommandsForRole role shipsAndCommands =
  filter (hasRole role) shipsAndCommands
    where
      hasRole wantRole (ShipAndCommands (Ship role _ _ _ _ _ _ _) _) = role == wantRole

getAttackerCommands ::GameState -> [Command]
getAttackerCommands (GameState _ _ shipsAndCommands) =
  map doIdle $ (getShipsAndCommandsForRole AttackerRole) shipsAndCommands
    where
      doIdle (ShipAndCommands (Ship _ shipId _ _ _ _ _ _) _) = AccelerateCommand shipId (1,0)


getDefenderCommands ::GameState -> [Command]
getDefenderCommands (GameState _ _ shipsAndCommands) =
  map doIdle $ (getShipsAndCommandsForRole DefenderRole) shipsAndCommands
    where
      doIdle (ShipAndCommands (Ship _ shipId _ _ _ _ _ _) _) = AccelerateCommand shipId (0,1)

