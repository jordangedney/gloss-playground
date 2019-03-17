{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
-- import Graphics.UI.GLUT.State (screenSize)
import Control.Lens
import Data.Fixed (mod'


                  )
import Data.List.Extra (nubOrd)
import Debug.Trace (trace)
import qualified Data.Map as Map
import Data.Maybe
-- import qualified Data.Set as Set

type Radius = Float
type Position = (Float, Float)
type Coordinates = (Float, Float)
type Width = Float
type Height = Float

data GameEntityAttributes = GameEntityAttributes
  { _velocity :: (Float, Float)
  , _position :: Position
  , _coords :: Coordinates
  } deriving (Show, Eq, Ord)
makeLenses ''GameEntityAttributes

data VisualAttributes = VisualAttributes
  { _objectColor :: Color
  , _displayed :: Bool
  } deriving (Show, Eq)
makeLenses ''VisualAttributes

data DimensionalAttributes
  = Sphere Radius
  | Rectangle Width Height
  deriving (Show, Eq, Ord)

data DisplayableShape = DisplayableShape
  { _visualAttrs :: VisualAttributes
  , _entityAttrs :: GameEntityAttributes
  , _dimensionalAttributes :: DimensionalAttributes
  } deriving (Show, Eq)
instance Ord DisplayableShape where
  (DisplayableShape _ e1 _) `compare` (DisplayableShape _ e2 _) = e1 `compare` e2
makeLenses ''DisplayableShape

-- | A structure to hold the state of the game
data GridGame = Game
  { _keysDown :: String
  , _entities :: [DisplayableShape]
  , _unprocessed :: Map.Map Coordinates DisplayableShape
  , _border :: Map.Map Coordinates DisplayableShape
  , _processed :: Map.Map Coordinates DisplayableShape
  , _edge :: [DisplayableShape]
  , _whites :: [DisplayableShape]
  , _secondsPlaying :: Float
  , _justUpdated :: Bool
  , _frameNumber :: Int
  } deriving (Show, Eq)
makeLenses ''GridGame

-- | Initial game state
initialState :: GridGame
initialState = Game
  { _keysDown = ""
  , _entities = allCubes
  , _unprocessed = unprocessed
  , _border = Map.fromList [((0,0), (fromJust $ Map.lookup (0,0) unprocessed))]
  , _processed = Map.empty
  , _edge = [x | x <- gridOfSquares, x^.entityAttrs.coords == (0, 0)]
  , _whites = []
  , _secondsPlaying = 0
  , _justUpdated = False
  , _frameNumber = 0
  }
  where square = DisplayableShape
          { _visualAttrs = VisualAttributes
            { _objectColor = greyN 0.2, _displayed = True }
          , _entityAttrs = GameEntityAttributes
            { _velocity = (0,0), _position = (0, 0), _coords = (0, 0)}
          , _dimensionalAttributes = (Rectangle width width)
          }
        numSquares = 100
        width = 5
        gap = (width / 5) + width
        updatePos x y (xPos, yPos) = (xPos + (gap * x), (yPos + gap * y))
        updateCoords x y (_, _) = (x, y)
        updatedCoords x y sqr = over (entityAttrs . coords) (updateCoords x y) sqr
        updatedPos x y sqr = over (entityAttrs . position) (updatePos x y) sqr
        newSquare x y = updatedCoords x y $ updatedPos x y square
        gridOfSquares = [newSquare x y |
                          x <- [-numSquares..numSquares],
                          y <- [-numSquares..numSquares]]
        whiteCenter sqr = if (view (entityAttrs . coords) sqr == (0, 0))
                          then over (visualAttrs . objectColor) (\_ -> white) sqr
                          else sqr
        allCubes = map whiteCenter gridOfSquares
        unprocessed = Map.fromList [(x^. entityAttrs . coords, x) | x <- allCubes]


colorAndMove :: Color -> Position -> Picture -> Picture
colorAndMove color_ position sprite =
  uncurry translate position $
  color color_ $
  sprite

displayEntity :: DisplayableShape -> Picture
displayEntity (DisplayableShape
                (VisualAttributes color_ _)
                (GameEntityAttributes _ position_ _)
                (Sphere radius)) =
  colorAndMove color_ position_ $
  circleSolid radius
displayEntity (DisplayableShape
                (VisualAttributes color_ _)
                (GameEntityAttributes _ position_ _)
                (Rectangle height width)) =
  colorAndMove color_ position_ $
  rectangleSolid width height

-- | Some gobals to share between rendering/collision code
movementSpeed :: Float
movementSpeed = 7

-- | Convert the game state into a picture
render :: GridGame -> Picture
render gameState = pictures
  [ pictures $  map displayEntity $ Map.elems (_unprocessed gameState)
  , pictures $  map displayEntity $ Map.elems (_border gameState)
  , pictures $  map displayEntity $ Map.elems (_processed gameState)
  ]

-- | Respond to key events
handleKeys :: Event -> GridGame -> GridGame
-- Ignore KeyState (up or down), Modifiers, and mouse position
handleKeys (EventKey (Char 'q') _ _ _) _ =
  error "Quit"

handleKeys (EventKey (Char char) Down _ _) game =
  game { _keysDown = (_keysDown game) ++ [char] }
handleKeys (EventKey (Char char) Up _ _) game =
  game { _keysDown = [ x | x <- (_keysDown game), not (x == char) ] }
handleKeys _ game = game

updateGameOnKeyDown :: Char -> GridGame -> GridGame
updateGameOnKeyDown _ game = game

neighborNodes :: [DisplayableShape] -> DisplayableShape -> [DisplayableShape]
neighborNodes nodeList node
  = [x | x <- nodeList,
      or [ x ^. entityAttrs . coords == (nodeX + 1, nodeY)
         , x ^. entityAttrs . coords == (nodeX - 1, nodeY)
         , x ^. entityAttrs . coords == (nodeX, nodeY + 1)
         , x ^. entityAttrs . coords == (nodeX, nodeY - 1)]]
  where nodeX = node ^. entityAttrs . coords . _1
        nodeY = node ^. entityAttrs . coords . _2
        right = node & entityAttrs . coords .~ (nodeX + 1, nodeY)
        left = node & entityAttrs . coords .~ (nodeX - 1, nodeY)
        up = node & entityAttrs . coords .~ (nodeX, nodeY + 1)
        down = node & entityAttrs . coords .~ (nodeX, nodeY - 1)


neighborNodes' :: Map.Map Coordinates DisplayableShape
               -> DisplayableShape
               -> Map.Map Coordinates DisplayableShape
neighborNodes' nodeList node
  = neighborCoords
  & map (\x -> Map.lookup x nodeList)
  & catMaybes
  & map (\x -> (x ^. entityAttrs . coords, x))
  & Map.fromList
  where (nodeX, nodeY) = (node ^. entityAttrs . coords . _1,
                          node ^. entityAttrs . coords . _2)
        rightCoords = (nodeX + 1, nodeY)
        leftCoords = (nodeX - 1, nodeY)
        upCoords = (nodeX, nodeY + 1)
        downCoords = (nodeX, nodeY - 1)
        neighborCoords = [rightCoords, leftCoords, upCoords, downCoords]

processWave :: GridGame -> GridGame
processWave gameState =
  gameState
  & addBorderToProcessed
  & expandBorder
  & removeBorderFromUnprocessed
  & over border (setColor white)
  & removeProcessedFromBorder
  & over processed (setColor $ greyN 0.2)
  where
        addBorderToProcessed game =
          game & processed .~
          (Map.union (game ^. processed) (game ^. border))

        expandBorder game =
          game & border .~
          Map.union (game ^. border) neighboringNodes
           where neighboringNodes =
                   (game ^. border)
                   & Map.elems
                   & map (neighborNodes' (game ^. unprocessed))
                   & Map.unions

        removeBorderFromUnprocessed game =
          game & unprocessed .~
          (Map.difference (game ^. unprocessed) (game ^. border))

        removeProcessedFromBorder game =
          game & border .~
          (Map.difference (game ^. border) (game ^. processed))

        setColor color_ = Map.map (\x -> x & visualAttrs . objectColor .~ color_)

spreadWhite :: GridGame -> GridGame
spreadWhite gameState =
  gameState
  & addEdgeToWhites
  & expandEdge
  & removeEdgeFromEntities
  & over edge (setColor white)
  & removeWhitesFromEdge
  & over whites (setColor $ greyN 0.2)
  where
        addEdgeToWhites game =
          game & whites .~
          game ^. edge ++ game ^. whites

        expandEdge game =
          game & edge .~
          (nubOrd $ concatMap (neighborNodes $ game ^. entities) $ game ^. edge)

        removeEdgeFromEntities game =
          game & entities .~
          [x | x <- (game ^. entities), not $ x `elem` game ^. edge]

        removeWhitesFromEdge game =
          game & edge .~
          [x | x <- game ^. edge, not $ x `elem` game ^. whites]

        setColor color_ = map (\x -> x & visualAttrs . objectColor .~ color_)

updateTotalSeconds :: Float -> GridGame -> GridGame
updateTotalSeconds secondsSinceLastUpdate game
  = over secondsPlaying (\s -> s + secondsSinceLastUpdate) game

everyXSeconds :: Int -> (GridGame -> GridGame) -> GridGame -> GridGame
everyXSeconds secondsInterval updateFn game =
  if and [seconds `rem` secondsInterval == 0
         , seconds /= 0]
  then if (not $ game ^. justUpdated)
       then (updateFn game) & justUpdated .~ True
       else game
  else game & justUpdated .~ False
  where seconds = floor $ view secondsPlaying game

everyXFrames :: Int -> (GridGame -> GridGame) -> GridGame -> GridGame
everyXFrames interval updateFn game =
  if game ^. frameNumber `mod` interval == 0
  then updateFn game
  else game

window :: Display
window = FullScreen

background :: Color
background = greyN 0.1

framesPerSecond :: Int
framesPerSecond = 60

quitIfDone :: GridGame -> GridGame
quitIfDone game =
  if length (game ^. edge) == 0
  then error "Finished"
  else game

debug :: GridGame -> GridGame
debug game = trace ("Entities : " ++
                    (show $ length $ game ^. entities) ++ "\n" ++
                    "Edge : " ++ (show $ length $ game ^. edge) ++ "\n" ++
                    "Whites : " ++ (show $ length $ game ^. whites))
             game

main :: IO ()
main = play window background framesPerSecond initialState render handleKeys update
  where
    update :: Float -> GridGame -> GridGame
    update secondsSinceLastUpdate
      = everyXFrames 30 processWave .
        debug .
        quitIfDone .
        (\g -> g & frameNumber .~ (1 + (g ^. frameNumber))) .
        updateTotalSeconds secondsSinceLastUpdate

-- bleh, fucking around with the screen size is aggrevating
-- main = do
  -- screenSize >>= mapM_ print
