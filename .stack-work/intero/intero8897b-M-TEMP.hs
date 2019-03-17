{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Lens

type Radius = Float
type Position = (Float, Float)
type Width = Float
type Height = Float

data GameEntityAttributes = GameEntityAttributes
  { _velocity :: (Float, Float)
  , _position :: (Float, Float)
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
  { _visualAttributes :: VisualAttributes
  , _gameEntityAttributes:: GameEntityAttributes
  , _dimensionalAttributes :: DimensionalAttributes
  } deriving (Show)
makeLenses ''DisplayableShape

-- | A structure to hold the state of the game
data PongGame = Game
  { _leftPlayer :: Float
  , _rightPlayer :: Float
  , _gamePaused :: Bool
  , _keysDown :: String
  , _ball :: DisplayableShape
  } deriving (Show)
makeLenses ''PongGame

-- | Initial game state
initialState :: PongGame
initialState = Game
  { _leftPlayer = 0
  , _rightPlayer = 0
  , _gamePaused = False
  , _keysDown = ""
  , _ball = initialBall
  }

initialBall :: DisplayableShape
initialBall = DisplayableShape
  { _visualAttributes = VisualAttributes
      { _objectColor = white, _displayed = True }
  , _gameEntityAttributes = GameEntityAttributes
      { _velocity = (400, 800), _position = (0, 0) }
  , _dimensionalAttributes = (Sphere 20)
  }

topWall :: DisplayableShape
topWall = DisplayableShape
  { _visualAttributes = VisualAttributes
      { _objectColor = greyN 0.2
      , _displayed = True }
  , _gameEntityAttributes =
      GameEntityAttributes { _velocity = (0, 0), _position = (0, wallCenter) }
  , _dimensionalAttributes = (Rectangle wallHeight wallWidth)
  }

bottomWall :: DisplayableShape
bottomWall = over (gameEntityAttributes . position) negateY topWall
  where negateY (xPos, yPos) = (xPos, (- yPos))

-- leftPlayer :: DisplayableShape
-- leftPlayer = DisplayableShape
  -- { _visualAttributes = VisualAttributes
      -- { _objectColor = light $ light $ light blue
      -- , _displayed = True }
  -- , _gameEntityAttributes =
      -- GameEntityAttributes { _velocity = (0, 0), _position = (playerDistanceFromCenter, 0) }
  -- , _dimensionalAttributes = (Rectangle playerWidth playerHeight)
  -- }

colorAndMove :: Color -> Position -> Picture -> Picture
colorAndMove color_ position sprite =
  uncurry translate position $
  color color_ $
  sprite

displayEntity :: DisplayableShape -> Picture
displayEntity (DisplayableShape
                (VisualAttributes color_ _)
                (GameEntityAttributes _ position_)
                (Sphere radius)) =
  colorAndMove color_ position_ $
  circleSolid radius
displayEntity (DisplayableShape
                (VisualAttributes color_ _)
                (GameEntityAttributes _ position_)
                (Rectangle height width)) =
  colorAndMove color_ position_ $
  rectangleSolid width height

-- | Some gobals to share between rendering/collision code
wallCenter :: Float
wallCenter = 600

wallHeight :: Float
wallHeight = 200

wallWidth :: Float
wallWidth = 6000

playerHeight :: Float
playerHeight = 200

playerWidth :: Float
playerWidth = 20

playerDistanceFromCenter :: Float
playerDistanceFromCenter = 1000

movementSpeed :: Float
movementSpeed = 7

-- | Convert the game state into a picture
render :: PongGame -> Picture
render gameState = pictures
  [ leftPaddle
  , rightPaddle
  , walls
  , ball_
  ]
  where
    walls = pictures [displayEntity topWall, displayEntity bottomWall]
    ball_ = displayEntity (_ball gameState)
    paddle = rectangleSolid playerWidth playerHeight

    lllblue = light $ light $ light blue
    ddblue = dark $ dark blue
    rightPaddle = translate playerDistanceFromCenter (_rightPlayer gameState) $
                 color ddblue $ paddle
    leftPaddle = translate (-playerDistanceFromCenter) (_leftPlayer gameState) $
                  color lllblue $ paddle

-- | Respond to key events
handleKeys :: Event -> PongGame -> PongGame
-- Ignore KeyState (up or down), Modifiers, and mouse position
handleKeys (EventKey (Char 'r') _ _ _) game =
  over (ball . gameEntityAttributes . position) zero game
  where zero _ = (0,0)

handleKeys (EventKey (Char 'p') Down _ _) game =
  game { _gamePaused = (not (_gamePaused game)) }

handleKeys (EventKey (Char 'q') _ _ _) _ =
  error "Quit"

handleKeys (EventKey (Char char) Down _ _) game =
  if (_gamePaused game) then game else
  game { _keysDown = (_keysDown game) ++ [char] }
handleKeys (EventKey (Char char) Up _ _) game =
  game { _keysDown = [ x | x <- (_keysDown game), not (x == char) ] }
handleKeys _ game = game

ballAttributes game =
  (xPos, yPos, xVel, yVel, radius)
  where (xPos, yPos) = view (ball . gameEntityAttributes . position) game
        (xVel, yVel) = view (ball . gameEntityAttributes . velocity) game
        radius = (\(Sphere r) -> r) $ view (ball . dimensionalAttributes) game

-- | Update the ball position using its velocity
moveBall :: Float    -- ^ The number of seconds since the last update
         -> PongGame -- ^ The initial game state
         -> PongGame -- ^ A new game state with an updated ball position
moveBall seconds game =
  over (ball . gameEntityAttributes . position) newPos game
  where (xPos, yPos, xVel, yVel, radius) = ballAttributes game
        newPos _ = (xPos + xVel * seconds, yPos + yVel * seconds)

-- | Detect a collision with one of the paddles. On collisions,
-- change the velocity of the vall to bounce it off the paddle.
paddleBounce :: PongGame -> PongGame
paddleBounce game =
  over (ball . gameEntityAttributes . velocity) newVel game
  where (xPos, yPos, xVel, yVel, radius) = ballAttributes game
        newVel _ = (newXVel, yVel)
        newXVel = if paddleCollision (xPos, yPos) radius
                       -- Invert y direction
                       then -xVel
                       -- Do nothing
                       else xVel

        paddleCollision :: Position -> Radius -> Bool
        paddleCollision (x, y) radius  = rightCollision || leftCollision
          where
                leftTop = (_leftPlayer game) + (playerHeight / 2)
                leftBottom = (_leftPlayer game) - (playerHeight / 2)
                leftRightSide =  (-playerDistanceFromCenter) + (playerWidth / 2)
                leftLeftSide =  (-playerDistanceFromCenter) - (playerWidth / 2)

                rightTop = (_rightPlayer game) + (playerHeight / 2)
                rightBottom = (_rightPlayer game) - (playerHeight / 2)
                rightRightSide = playerDistanceFromCenter + (playerWidth / 2)
                rightLeftSide = playerDistanceFromCenter - (playerWidth / 2)

                leftCollision = x + radius >= leftLeftSide &&
                                x - radius <= leftRightSide &&
                                y + radius <= leftTop  &&
                                y - radius >= leftBottom

                rightCollision = x - radius <= rightRightSide &&
                                 x + radius >= rightLeftSide &&
                                 y + radius <= rightTop  &&
                                 y - radius >= rightBottom

-- | Detect a collision with one of the walls. On collisions,
-- change the velocity of the vall to bounce it off the paddle.
wallBounce :: PongGame -> PongGame
wallBounce game =
  over (ball . gameEntityAttributes . velocity) newVel game
  where (xPos, yPos, xVel, yVel, radius) = ballAttributes game
        newVel _ = (xVel, newYVel)
        newYVel = if wallCollision (xPos, yPos) radius
                  -- Invert y direction
                  then -yVel
                  -- Do nothing
                  else yVel

        wallCollision :: Position -> Radius -> Bool
        wallCollision (_, y) radius = topCollision || bottomCollision
          where wallBorder = wallCenter - (wallHeight / 2)
                topCollision = y - radius <= -wallBorder
                bottomCollision = y + radius >= wallBorder


updateGameOnKeyDown :: Char -> PongGame -> PongGame
updateGameOnKeyDown 'd' game =
  game { _leftPlayer = (_leftPlayer game) + movementSpeed }
updateGameOnKeyDown 'f' game =
  game { _leftPlayer = (_leftPlayer game) - movementSpeed }
updateGameOnKeyDown 'j' game =
  game { _rightPlayer = (_rightPlayer game) - movementSpeed }
updateGameOnKeyDown 'k' game =
  game { _rightPlayer = (_rightPlayer game) + movementSpeed }
updateGameOnKeyDown _ game = game

updatePaddles :: PongGame -> PongGame
updatePaddles game =
  foldl (\game_ char -> updateGameOnKeyDown char game_) game (_keysDown game)

haltIfPaused :: Float -> PongGame -> PongGame
haltIfPaused seconds game =
  if (_gamePaused game)
  then game
  else updatePaddles . paddleBounce . wallBounce . moveBall seconds $ game

window :: Display
window = FullScreen
--  window = InWindow "Pong" (500, 500) (1, 1)

background :: Color
background = greyN 0.1

framesPerSecond :: Int
framesPerSecond = 60

main :: IO ()
main = play window background framesPerSecond initialState render handleKeys update
  where
    update :: Float -> PongGame -> PongGame
    update seconds = haltIfPaused seconds
