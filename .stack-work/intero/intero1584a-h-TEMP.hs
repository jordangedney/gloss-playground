{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import qualified Queue as Q
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
-- import Graphics.UI.GLUT.State (screenSize)
import Control.Lens
import Data.Fixed (mod')
import Data.List.Extra (nubOrd)
import Debug.Trace (trace)
import qualified Data.Map as Map
import Data.Maybe

type Radius = Float
type Position = (Float, Float)
type Coordinates = (Float, Float)
type Width = Float
type Height = Float

runSpeed = 20
numberSquares = 90
widthSquares = 10
gapSquares = (widthSquares / 2) + widthSquares
-- gapSquares = widthSquares

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
  , _unprocessed :: Map.Map Coordinates DisplayableShape
  , _frontier :: Map.Map Coordinates DisplayableShape
  , _processed :: Map.Map Coordinates DisplayableShape
  , _wall :: Map.Map Coordinates DisplayableShape
  , _nodesToProcess :: Q.Queue DisplayableShape
  , _frameNumber :: Int
  } deriving (Show)
makeLenses ''GridGame

toSet nodes = Map.fromList [(x ^. entityAttrs . coords, x) | x <- nodes]

-- | Initial game state
initialState :: GridGame
initialState = Game
  { _keysDown = ""
  , _unprocessed = unprocessed
  , _frontier = center
  , _processed = Map.empty
  , _wall = coloredWall
  , _nodesToProcess = Q.push (fromJust (Map.lookup (0, 0) center)) Q.emptyQueue
  , _frameNumber = 0
  }
  where square = DisplayableShape
          { _visualAttrs = VisualAttributes
            { _objectColor = greyN 0.2, _displayed = True }
          , _entityAttrs = GameEntityAttributes
            { _velocity = (0,0), _position = (0, 0), _coords = (0, 0)}
          , _dimensionalAttributes = (Rectangle width width)
          }
        numSquares = numberSquares
        width = widthSquares
        gap = gapSquares

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
        allMap = toSet allCubes
        (center, rest) = Map.partitionWithKey (\k _ -> k == (0,0)) allMap
        (wall, unprocessed) = Map.partitionWithKey (\k _ -> k `elem` wallNodes) rest
        coloredWall = Map.map (\n -> n & (visualAttrs . objectColor) .~ (greyN 0.1)) wall

        wallTop = [(x, y) | x <- [-10..10], y <- [10..15]]
        wallBottom = [(x, y) | x <- [-10..10], y <- [(-15)..(-10)]]
        wallRight = [(x, y) | x <- [15..20], y <- [(-10)..10]]
        wallNodes = foldr1 (++) [wallTop, wallBottom, wallRight]


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

-- | Convert the game state into a picture
render :: GridGame -> Picture
render gameState = pictures
  [ pictures $  map displayEntity $ Map.elems (_unprocessed gameState)
  , pictures $  map displayEntity $ Map.elems (_frontier gameState)
  , pictures $  map displayEntity $ Map.elems (_processed gameState)
  , pictures $  map displayEntity $ Map.elems (_wall gameState)
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

neighborNodes :: Map.Map Coordinates DisplayableShape
               -> DisplayableShape
               -> Map.Map Coordinates DisplayableShape
neighborNodes nodeList node
  = neighborCoords
  & map (\x -> Map.lookup x nodeList)
  & catMaybes
  & toSet
  where (nodeX, nodeY) = (node ^. entityAttrs . coords . _1,
                          node ^. entityAttrs . coords . _2)
        rightCoords = (nodeX + 1, nodeY)
        leftCoords = (nodeX - 1, nodeY)
        upCoords = (nodeX, nodeY + 1)
        downCoords = (nodeX, nodeY - 1)
        neighborCoords = [rightCoords, leftCoords, upCoords, downCoords]

breadthFirstSearch :: GridGame -> GridGame
breadthFirstSearch gameState =
  case top of
    Nothing -> gameState
    Just element ->
      gameState
      & replaceQueue
      & addFrontierToProcessed
      where
            replaceQueue game = game & nodesToProcess .~ newQueue

            addFrontierToProcessed game =
              game & processed .~
              (Map.union (game ^. processed) $ toSet [element])

  where (top, newQueue) = Q.pop $ gameState ^. nodesToProcess


processWave :: GridGame -> GridGame
processWave gameState =
  gameState
  & addFrontierToProcessed
  & expandFrontier
  & removeFrontierFromUnprocessed
  & over frontier (setColor white)
  & removeProcessedFromFrontier
  & over processed (setColor $ greyN 0.2)
  where
        addFrontierToProcessed game =
          game & processed .~
          (Map.union (game ^. processed) (game ^. frontier))

        expandFrontier game =
          game & frontier .~
          Map.union (game ^. frontier) neighboringNodes
           where neighboringNodes =
                   (game ^. frontier)
                   & Map.elems
                   & map (neighborNodes (game ^. unprocessed))
                   & Map.unions

        removeFrontierFromUnprocessed game =
          game & unprocessed .~
          (Map.difference (game ^. unprocessed) (game ^. frontier))

        removeProcessedFromFrontier game =
          game & frontier .~
          (Map.difference (game ^. frontier) (game ^. processed))

        setColor color_ = Map.map (\x -> x & visualAttrs . objectColor .~ color_)

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

-- quitIfDone :: GridGame -> GridGame
-- quitIfDone game =
  -- if length (game ^. edge) == 0
  -- then error "Finished"
  -- else game

-- debug :: GridGame -> GridGame
-- debug game = trace ("Entities : " ++
                    -- (show $ length $ game ^. entities) ++ "\n" ++
                    -- "Edge : " ++ (show $ length $ game ^. edge) ++ "\n" ++
                    -- "Whites : " ++ (show $ length $ game ^. whites))
             -- game

main :: IO ()
main = play window background framesPerSecond initialState render handleKeys update
  where
    update :: Float -> GridGame -> GridGame
    update secondsSinceLastUpdate
      = everyXFrames runSpeed processWave .
        -- debug .
        -- quitIfDone .
        (\g -> g & frameNumber .~ (1 + (g ^. frameNumber)))

-- bleh, fucking around with the screen size is aggrevating
-- main = do
  -- screenSize >>= mapM_ print
