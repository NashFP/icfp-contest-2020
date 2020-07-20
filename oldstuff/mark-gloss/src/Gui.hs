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

data World = World
  { changed :: Bool
  , state :: Value
  , bitmaps :: [[(Integer,Integer)]]
  , clickPoint :: (Integer,Integer)
  , minBounds :: (Integer,Integer)
  , maxBounds ::(Integer,Integer)
  , pics :: [Picture]
  }

createWorldStep :: World -> String
createWorldStep (World { state = NilValue, clickPoint = (x, y) }) =
  "ap ap ap interact galaxy nil ap ap cons " ++ show x ++ " " ++ show y

createWorldStep (World { state = state, clickPoint = (x, y) }) =
  "ap ap ap interact galaxy ap dem %" ++ (show $ encode state) ++ " ap ap cons " ++ show x ++ " " ++ show y

windowCoordsToGalactic :: (Integer, Integer) -> (Float, Float) -> (Integer, Integer)
windowCoordsToGalactic (minX, minY) (xf, yf) =
  (minX + ((round xf) + 550) `div` 3,
   minY - 1 + ((round (-yf)) + 350) `div` 3)

galacticCoordsToWindow :: (Integer, Integer) -> (Integer, Integer) -> (Float, Float)
galacticCoordsToWindow (minX, minY) (x, y) =
  (fromInteger (3 * (x - minX) - 550),
   0 - fromInteger (3 * (y - minY) - 350))

generatePixel :: (Integer,Integer) -> ((Integer,Integer),Color) -> Picture
generatePixel minBounds ((x,y),col) =
  let (xf, yf) = galacticCoordsToWindow minBounds (x, y)
  in color col $ translate xf yf $ rectangleSolid 3.0 3.0

type PixelMap = [((Integer, Integer), Color)]

addBitmapToPixelMap :: ([(Integer,Integer)], Color) -> PixelMap -> PixelMap
addBitmapToPixelMap (coords, color) pixelMap =
  foldl' addPixel pixelMap coords
  where
    addPixel pm coord = (coord, color) : pm
    keepOld n o = o

addBitmapsToPixelMap :: [[(Integer,Integer)]] -> [Color] -> PixelMap -> PixelMap
addBitmapsToPixelMap frames colors pixelMap =
  foldr addBitmapToPixelMap pixelMap (reverse (zip frames colors))

goodColors :: [Color]
goodColors = map (\x -> makeColor (x ** 1.7) x (x ** 0.75) 1.0) $ iterate (* 0.65) 1.0

worldToPic :: Env -> World -> Picture
worldToPic _ (World { bitmaps = bitmaps, minBounds = (minX, minY), pics = []}) =
  let pixelMap = addBitmapsToPixelMap bitmaps goodColors [] in
  Pictures $ map (generatePixel (minX,minY)) pixelMap
worldToPic _ (World { pics = pics }) = Pictures pics

eventHandler :: Env -> Event -> World -> World
eventHandler env (EventKey (MouseButton LeftButton) Up _ (xf, yf)) world =
  let (x, y) = windowCoordsToGalactic (minBounds world) (xf, yf)
  in trace ("click coords = "++show x++","++show y)
       (world { changed = True, clickPoint = (x, y) })
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
iterateWorld env _ world@(World {changed=False}) = world
iterateWorld env _ world =
  let worldStep = createWorldStep world in
  case parse replEntry "<stdin>" worldStep of
    Left x -> error $ show x
    Right value ->
      let v = eval env value in
      trace ("Got response "++show v++"  length "++show (length (valueToList v))) (
      let (newState:bitmapsValue) = valueToList v
          bitmaps = filter (\l -> (length l) > 0) $ map (\b -> map valueToCoordinatePair (valueToList b)) bitmapsValue
          (minBounds,maxBounds) = computeAllImageBounds bitmaps
      in World { changed=False, state=newState, bitmaps=bitmaps, clickPoint=clickPoint world,
                 minBounds=minBounds, maxBounds=maxBounds, pics=[] }
      )


runApp :: Env -> IO ()
runApp env = do
  let startWorld = iterateWorld env 0.0 (
        World { changed=True, state=NilValue, bitmaps=[], clickPoint=(0,0),
                minBounds=(0,0), maxBounds=(0,0), pics=[] })

  play (InWindow "NashFP Alien Clicker" (1200, 800) (10,10)) black 1 startWorld (worldToPic env) (eventHandler env) (iterateWorld env)
