module Gui where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import DataTypes
import Eval
import Encode
import Parse
import qualified Data.Map.Strict as Map
import Data.List
import Debug.Trace

data World =
  World Bool Value [[(Integer,Integer)]] (Integer,Integer) (Integer,Integer) (Integer,Integer) [Picture]

createWorldStep :: World -> String
createWorldStep (World _ NilValue _ (x,y) _ _ _) =
  "ap ap ap interact galaxy nil ap ap cons " ++ show x ++ " " ++ show y

createWorldStep (World _ state _ (x,y) _ _ _) =
  "ap ap ap interact galaxy ap dem %" ++ (show $ encode state) ++ " ap ap cons " ++ show x ++ " " ++ show y

generatePixel :: (Integer,Integer) -> ((Integer,Integer),Color) -> Picture
generatePixel (minX,minY) ((x,y),col) =
  color col $ translate (fromInteger ((3 * (x-minX)-550))) (fromInteger ((3 * (y-minY))-350)) (rectangleSolid 3.0 3.0)

addBitmapToPixelMap :: [(Integer,Integer)] -> Color -> Map.Map (Integer,Integer) Color -> Map.Map (Integer,Integer) Color
addBitmapToPixelMap coords color pixelMap =
  foldl' addPixel pixelMap coords
  where
    addPixel pm coord = Map.insertWith keepOld coord color pm
    keepOld n o = o

addBitmapsToPixelMap :: [[(Integer,Integer)]] -> [Color] -> Map.Map (Integer,Integer) Color -> Map.Map (Integer,Integer) Color
addBitmapsToPixelMap [] _ pixelMap = pixelMap
addBitmapsToPixelMap (b:bs) (color:colors) pixelMap =
  addBitmapsToPixelMap bs colors $
    addBitmapToPixelMap b color pixelMap

worldToPic :: Env -> World -> Picture
worldToPic _ (World _ _ bitmaps _ (minX,minY) _ []) =
  let pixelMap = addBitmapsToPixelMap bitmaps (cycle [blue,red,green,magenta,cyan,white]) Map.empty in
  Pictures $ map (generatePixel (minX,minY)) (Map.assocs pixelMap)
worldToPic _ (World _ _ _ _ _ _ pics) = Pictures pics

eventHandler :: Env -> Event -> World -> World
eventHandler env (EventKey (MouseButton LeftButton) Up _ (xf,yf)) (World _ state bitmaps _ (minX,minY) maxBounds pics) =
  let x = ((round xf) + 550) `div` 3 in
  let y = 1 + ((round yf) + 350) `div` 3 in
  trace ("click coords = "++show (x+minX)++","++show (y+minY)) (
  World True state bitmaps (x+minX,y+minY) (minX,minY) maxBounds pics
  )
eventHandler env _ world = world

computeImageBounds :: [(Integer,Integer)] -> [Integer]
computeImageBounds pairs =
  let minX = minimum $ fmap fst pairs
      maxX = maximum $ fmap fst pairs
      minY = minimum $ fmap snd pairs
      maxY = maximum $ fmap snd pairs
  in [minX,maxX,minY,maxY]

computeAllImageBounds :: [[(Integer,Integer)]] -> ((Integer,Integer),(Integer,Integer))
computeAllImageBounds x =
  let allBounds = map computeImageBounds x in
    let minX = minimum $ fmap (\p -> head p) allBounds
        maxX = minimum $ fmap (\p -> p !! 1) allBounds
        minY = minimum $ fmap (\p -> p !! 2) allBounds
        maxY = minimum $ fmap (\p -> p !! 3) allBounds
    in ((minX,minY),(maxX,maxY))


iterateWorld :: Env -> Float -> World -> World
iterateWorld env _ (World changed state bitmaps (x,y) minBounds maxBounds pics) =
  if not changed then
    World False state bitmaps (x,y) minBounds maxBounds pics
  else
    let worldStep = createWorldStep (World False state bitmaps (x,y) minBounds maxBounds pics) in
    case parse replEntry "<stdin>" worldStep of
      Left x -> error $ show x
      Right value ->
        let v = eval env value in
        trace ("Got response "++show v++"  length "++show (length (valueToList v))) (
        let (newState:bitmapsValue) = valueToList v in
        let bitmaps = filter (\l -> (length l) > 0) $ map (\b -> map valueToCoordinatePair (valueToList b)) bitmapsValue in
        let (minBounds,maxBounds) = computeAllImageBounds bitmaps in
        World False newState bitmaps (x,y) minBounds maxBounds []
        )


runApp :: Env -> IO ()
runApp env = do
  let startWorld = iterateWorld env 0.0 (World True NilValue [] (0,0) (0,0) (0,0) [])

  play (InWindow "NashFP Alien Clicker" (1200, 800) (10,10)) black 1 startWorld (worldToPic env) (eventHandler env) (iterateWorld env)