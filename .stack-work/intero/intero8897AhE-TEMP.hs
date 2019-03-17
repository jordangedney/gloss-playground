{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
-- import Graphics.UI.GLUT.State (screenSize)
import Control.Lens

type Radius = Float
type Position = (Float, Float)
type Width = Float
type Height = Float

data GameEntityAttributes = GameEntityAttributes
  { _velocity :: (Float, Float)
  , _position :: (Float, Float)
  , _coords :: (Float, Float)
  } deriving (Show)
makeLenses ''GameEntityAttributes

data VisualAttributes = VisualAttributes
  { _objectColor :: Color
  , _displayed :: Bool
  } deriving (Show)
makeLenses ''VisualAttributes

data DimensionalAttributes
  = Sphere Radius
  | Rectangle Width Height
  deriving (Show)

data DisplayableShape = DisplayableShape
  { _visualAttrs :: VisualAttributes
  , _entityAttrs:: GameEntityAttributes
  , _dimensionalAttributes :: DimensionalAttributes
  } deriving (Show)
makeLenses ''DisplayableShape

-- | A structure to hold the state of the game
data GridGame = Game
  { _entities:: [DisplayableShape]
  , _keysDown :: String
  } deriving (Show)
makeLenses ''GridGame

-- | Initial game state
initialState :: GridGame
initialState = Game
  { _keysDown = ""
  , _entities = map whiteCenter gridOfSquares
  }
  where square = DisplayableShape
          { _visualAttrs = VisualAttributes
            { _objectColor = greyN 0.2, _displayed = True }
          , _entityAttrs = GameEntityAttributes
            { _velocity = (0,0), _position = (0, 0), _coords = (0, 0)}
          , _dimensionalAttributes = (Rectangle width width)
          }
        numSquares = 100
        width = 10
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
  [ pictures $ map displayEntity (_entities gameState)
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

updatePaddles :: GridGame -> GridGame
updatePaddles game =
  foldl (\game_ char -> updateGameOnKeyDown char game_) game (_keysDown game)

window :: Display
window = FullScreen

background :: Color
background = greyN 0.1

framesPerSecond :: Int
framesPerSecond = 60

main :: IO ()
main = play window background framesPerSecond initialState render handleKeys update
  where
    update :: Float -> GridGame -> GridGame
    update seconds = updatePaddles

-- bleh, fucking around with the screen size is aggrevating
-- main = do
  -- screenSize >>= mapM_ print
