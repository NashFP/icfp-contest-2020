module GameDriver where

import DataTypes
import Comm
import GameLogic
import Attacker
import Defender
import Debug.Trace

doCreate url playerKey = do
  let req = ListValue [IntValue 1, IntValue 0]
  resp <- sendToServer url playerKey req
  case resp of
    ListValue [ IntValue 1, ListValue [ListValue [IntValue 0, IntValue attackerPlayerKey], ListValue [IntValue 1, IntValue defenderPlayerKey]]] -> do
      putStrLn $ "attacker player key = " ++ show attackerPlayerKey
      putStrLn $ "defender player key = " ++ show defenderPlayerKey
      writeFile "defender.key" (show defenderPlayerKey)
      writeFile "attacker.key" (show attackerPlayerKey)
    _ -> error $ "Unexpected reply from server "++show resp

parseGameInfo :: AlienData -> GameInfo
parseGameInfo (ListValue [x0, role, x2, x3, x4]) =
  GameInfo x0 (parseRole role) x2 x3 x4
parseGameInfo x = error $ "Unable to parse game info "++show x

parseGameState :: AlienData -> GameState
parseGameState (ListValue []) = GameState 0 (IntValue 0) []
parseGameState (ListValue [IntValue ticks, x1, shipsAndCommands]) =
  GameState ticks x1 (parseShipsAndCommands shipsAndCommands)
parseGameState x = error $ "Unable to parse game state "++show x

parseShipsAndCommands :: AlienData -> [ShipAndCommands]
parseShipsAndCommands (ListValue shipsAndCommands) =
  map parseShipAndCommands shipsAndCommands
parseShipsAndCommands x = error $ "Expected list of ships and commands, got "++show x

parseRole :: AlienData -> Role
parseRole (IntValue 0) = AttackerRole
parseRole (IntValue 1) = DefenderRole
parseRole x = error $ "Expected role, got "++show x

parseShip :: AlienData -> Ship
parseShip (ListValue (role:IntValue shipId:position:velocity:x4:x5:x6:x7:rest)) =
  Ship (parseRole role) shipId (parsePair position) (parsePair velocity) x4 x5 x6 x7
parseShip x = error $ "Expected ship, got "++show x

parseShipAndCommands :: AlienData -> ShipAndCommands
parseShipAndCommands (ListValue (ship:ListValue commands:[])) =
  trace ("parseShipAndCommands commands="++show commands) (
  let (Ship role shipId position velocity x4 x5 x6 x7) = parseShip ship in
    ShipAndCommands (Ship role shipId position velocity x4 x5 x6 x7) (map (parseShipCommand shipId) commands)
  )
parseShipAndCommands x = error $ "Expected list with ship and commands, got "++show x

parseInt :: AlienData -> Integer
parseInt (IntValue i) = i
parseInt x = error $ "Expected integer in vector got "++show x

parseVector :: AlienData -> [Integer]
parseVector (ListValue values) = map parseInt values
parseVector x = error $ "Expected vector, got "++show x

parsePair :: AlienData -> (Integer,Integer)
parsePair (PairValue (x,y)) = (x,y)
parsePair x = error $ "Expected pair, got "++show x

parseCommand :: AlienData -> Command
parseCommand (ListValue (IntValue 0:IntValue shipId:pair:_)) =
  AccelerateCommand shipId (parsePair pair)
parseCommand (ListValue (IntValue 1:IntValue shipId:_)) = DetonateCommand shipId
parseCommand (ListValue (IntValue 2:IntValue shipId:target:IntValue x1:IntValue x2:IntValue x3:rest)) =
  ShootCommand shipId (parsePair target) x1 x2 x3
parseCommand (ListValue (IntValue commandId:IntValue shipId:rest)) = UnknownCommand commandId shipId rest
parseCommand (ListValue []) = UnknownCommand 0 0 []
parseCommand x = error $ "Expected command, got "++show x

parseShipCommand :: Integer -> AlienData -> Command
parseShipCommand shipId (ListValue (IntValue 0:pair:_)) =
  AccelerateCommand shipId (parsePair pair)
parseShipCommand shipId (ListValue (IntValue 1:_)) = DetonateCommand shipId
parseShipCommand shipId (ListValue (IntValue 2:target:IntValue x1:IntValue x2:IntValue x3:_)) =
  ShootCommand shipId (parsePair target) x1 x2 x3
parseShipCommand shipId (ListValue (IntValue commandId:rest)) = UnknownCommand commandId shipId rest
parseShipCommand shipId (ListValue []) = UnknownCommand 0 shipId []
parseShipCommand shipId x = error $ "Expected ship command, got "++show x

unparseVector :: [Integer] -> AlienData
unparseVector v = ListValue $ map IntValue v

unparsePair :: (Integer,Integer) -> AlienData
unparsePair (x,y) = PairValue (x,y)

unparseCommand:: Command -> AlienData
unparseCommand (AccelerateCommand shipId vec) =
  ListValue (IntValue 0:IntValue shipId:(unparsePair vec):[])
unparseCommand (DetonateCommand shipId) =
  ListValue (IntValue 1:IntValue shipId:[])
unparseCommand (ShootCommand shipId target x1 x2 x3) =
  ListValue (IntValue 2:IntValue shipId:(unparsePair target):IntValue x1:IntValue x2:IntValue x3:[])

parseGameStage :: AlienData -> GameStage
parseGameStage (IntValue 0) = GameNotStarted
parseGameStage (IntValue 1) = GameStarted
parseGameStage (IntValue 2) = GameFinished
parseGameStage x = error $ "Expected game stage (0-2), got "++show x

parseResponse :: AlienData -> GameResponse
parseResponse (ListValue (IntValue 0:x)) = InvalidResponse
parseResponse (ListValue (IntValue 1:gameStage:staticGameInfo:gameState:x)) =
  ValidResponse (parseGameStage gameStage) (parseGameInfo staticGameInfo) (parseGameState gameState)
parseResponse x= error $ "Expected game response, got "++show x

sendJoin :: String -> String -> IO GameResponse
sendJoin url playerKey = do
  let joinCommand = ListValue [IntValue 2, IntValue (read playerKey::Integer), ListValue []]
  response <- sendToServer url playerKey joinCommand
  return $ parseResponse response

sendStart :: String -> String -> IO GameResponse
sendStart url playerKey = do
  let joinCommand = ListValue [IntValue 3, IntValue (read playerKey::Integer),
       ListValue [IntValue 300,IntValue 4,IntValue 10,IntValue 2]]
  response <- sendToServer url playerKey joinCommand
  return $ parseResponse response

sendCommands :: String -> String -> [Command] -> IO GameResponse
sendCommands url playerKey commands = do
  let commandsCommand = ListValue [IntValue 4, IntValue (read playerKey::Integer),
       ListValue (map unparseCommand commands)]
  response <- sendToServer url playerKey commandsCommand
  return $ parseResponse response


startGame url playerKey = do
  joinResp <- sendJoin url playerKey
  case joinResp of
    InvalidResponse -> error "Got invalid response after join"
    joinResp -> do
      startResp <- sendStart url playerKey
      case startResp of
        InvalidResponse -> error "Error got invalid response after start"
        ValidResponse _ (GameInfo _ role _ _ _) gameState ->
          runGame url playerKey role gameState


runGame url playerKey role gameState = do
  putStrLn $ "Current game state = " ++ show gameState
  let commands = if role == AttackerRole then
                  getAttackerCommands gameState
                else
                  getDefenderCommands gameState

  putStrLn $ "Sending commands " ++ show commands
  commandsResp <- sendCommands url playerKey commands
  putStrLn $ "Got command response " ++ show commandsResp
  case commandsResp of
    InvalidResponse -> error "Got invalid response for commands"
    ValidResponse gameStage _ newGameState ->
      if gameStage == GameFinished then
        putStrLn "Game finished"
      else
        runGame url playerKey role newGameState


