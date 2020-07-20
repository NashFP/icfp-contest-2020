module DataTypes where

data AlienData =
  IntValue Integer |
  ListValue [AlienData]
  deriving (Eq,Show)

data Role = AttackerRole | DefenderRole
  deriving (Eq,Show)

data GameInfo = GameInfo AlienData Role AlienData AlienData AlienData
  deriving (Eq,Show)

data Ship = Ship Role Integer [Integer] [Integer] AlienData AlienData AlienData AlienData
  deriving (Eq,Show)

data Command = AccelerateCommand Integer [Integer] |
  DetonateCommand Integer |
  ShootCommand Integer [Integer] AlienData |
  UnknownCommand Integer Integer [AlienData]
  deriving (Eq,Show)

data ShipAndCommands = ShipAndCommands Ship [Command]
  deriving (Eq,Show)

data GameState = GameState Integer AlienData [ShipAndCommands]
  deriving (Eq,Show)

data GameStage = GameNotStarted | GameStarted | GameFinished
  deriving (Eq,Show)

data GameResponse = InvalidResponse | ValidResponse GameStage GameInfo GameState
  deriving (Eq,Show)


