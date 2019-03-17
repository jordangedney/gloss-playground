module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

type Radius = Float
type Position = (Float, Float)
type Width = Float
type Height = Float

-- | A structure to hold the state of the game
data PongGame = Game
  { ballLocation :: (Float, Float)
  , ballVelocity :: (Float, Float)
  , leftPlayer :: Float
  , rightPlayer :: Float
  , gamePaused :: Bool
  , keysDown :: String
  , ball :: DisplayableShape
  } deriving (Show)

-- | Initial game state
initialState :: PongGame
initialState = Game
  { ballLocation = (0, 0)
  , ballVelocity = (400, 800)
  , leftPlayer = 0
  , rightPlayer = 0
  , gamePaused = False
  , keysDown = ""
  , ball = initialBall
  }

data GameEntityAttributes = GameEntitiyAttributes
  { velocity :: (Float, Float)
  , position :: (Float, Float)
  } deriving (Show)

data VisualAttributes = VisualAttributes
  { objectColor :: Color
  , displayed :: Bool
  } deriving (Show)

data DisplayableShape
  = Sphere VisualAttributes GameEntityAttributes Radius
  | Square VisualAttributes GameEntityAttributes Width Height
  deriving (Show)

initialBall :: DisplayableShape
initialBall =
  Sphere
  VisualAttributes { objectColor = white, displayed = True }
  GameEntitiyAttributes { velocity = (400, 800), position = (0, 0) }
  20

colorAndMove :: Color -> Position -> Picture -> Picture
colorAndMove color_ position sprite =
  uncurry translate position $
  color color_ $
  sprite

displayEntitiy :: DisplayableShape -> Picture
displayEntitiy (Sphere visuals gameProperties radius) =
  colorAndMove (objectColor visuals) (position gameProperties) $
  circleSolid radius

-- | Some gobals to share between rendering/collision code
wallCenter :: Float
wallCenter = 600

ballSize :: Float
ballSize = 10

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
  , ball
  ]
  where
    ball = displayEntitiy initialBall
    paddle = rectangleSolid playerWidth playerHeight
    lllblue = light $ light $ light blue
    ddblue = dark $ dark blue
    rightPaddle = translate playerDistanceFromCenter (rightPlayer gameState) $
                 color ddblue $ paddle
    leftPaddle = translate (-playerDistanceFromCenter) (leftPlayer gameState) $
                  color lllblue $ paddle
    wall = color (greyN 0.2) $ rectangleSolid wallWidth wallHeight
    topWall = translate 0 wallCenter $ wall
    bottomWall = translate 0 (-wallCenter) $ wall
    walls = pictures [topWall, bottomWall]


-- | Respond to key events
handleKeys :: Event -> PongGame -> PongGame
-- Ignore KeyState (up or down), Modifiers, and mouse position
handleKeys (EventKey (Char 'r') _ _ _) game =
  game { ball = ballAtStart }
  where ballAtStart = zeroed $ ball game
        zeroed (Sphere vAttrs gAttrs radius) =
          (Sphere vAttrs (gAttrs {position = (0, 0)}) radius)

handleKeys (EventKey (Char 'p') Down _ _) game =
  game { gamePaused = (not (gamePaused game)) }

handleKeys (EventKey (Char 'q') _ _ _) _ =
  error "Quit"

handleKeys (EventKey (Char char) Down _ _) game =
  if (gamePaused game) then game else
  game { keysDown = (keysDown game) ++ [char] }
handleKeys (EventKey (Char char) Up _ _) game =
  game { keysDown = [ x | x <- (keysDown game), not (x == char) ] }


-- handleKeys (EventKey (Char 'k') Down _ _) game =
--   if (gamePaused game) then game else
--   game { rightPlayer = previousY + movementSpeed }
--   where previousY = (rightPlayer game)
-- handleKeys (EventKey (Char 'j') Down _ _) game =
--   if (gamePaused game) then game else
--   game { rightPlayer = previousY - movementSpeed }
--   where previousY = (rightPlayer game)

handleKeys _ game = game


-- | Update the ball position using its velocity
moveBall :: Float    -- ^ The number of seconds since the last update
         -> PongGame -- ^ The initial game state
         -> PongGame -- ^ A new game state with an updated ball position
moveBall seconds gameState =
  gameState { ballLocation = (newX, newY) }
  where (previousX, previousY) = ballLocation gameState
        (xVelocity, yVelocity) = ballVelocity gameState

        newX = previousX + xVelocity * seconds
        newY = previousY + yVelocity * seconds

-- | Detect a collision with one of the paddles. On collisions,
-- change the velocity of the vall to bounce it off the paddle.
paddleBounce :: PongGame -> PongGame
paddleBounce game =
  game { ballVelocity = (newXVelocity, previousYVelocity) }
  where (previousXVelocity, previousYVelocity) = ballVelocity game

        paddleCollision :: Position -> Radius -> Bool
        paddleCollision (x, y) radius  = rightCollision || leftCollision
          where
                leftTop = (leftPlayer game) + (playerHeight / 2)
                leftBottom = (leftPlayer game) - (playerHeight / 2)
                leftRightSide =  (-playerDistanceFromCenter) + (playerWidth / 2)
                leftLeftSide =  (-playerDistanceFromCenter) - (playerWidth / 2)

                rightTop = (rightPlayer game) + (playerHeight / 2)
                rightBottom = (rightPlayer game) - (playerHeight / 2)
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

        newXVelocity = if paddleCollision (ballLocation game) ballSize
                       -- Invert y direction
                       then -previousXVelocity
                       -- Do nothing
                       else previousXVelocity

-- | Detect a collision with one of the walls. On collisions,
-- change the velocity of the vall to bounce it off the paddle.
wallBounce :: PongGame -> PongGame
wallBounce game =
  game { ballVelocity = (previousXVelocity, newYVelocity) }
  where (previousXVelocity, previousYVelocity) = ballVelocity game

        wallCollision :: Position -> Radius -> Bool
        wallCollision (_, y) radius = topCollision || bottomCollision
          where wallBorder = wallCenter - (wallHeight / 2)
                topCollision = y - radius <= -wallBorder
                bottomCollision = y + radius >= wallBorder

        newYVelocity = if wallCollision (ballLocation game) ballSize
                       -- Invert y direction
                       then -previousYVelocity
                       -- Do nothing
                       else previousYVelocity


updateGameOnKeyDown :: Char -> PongGame -> PongGame
updateGameOnKeyDown 'd' game =
  game { leftPlayer = (leftPlayer game) + movementSpeed }
updateGameOnKeyDown 'f' game =
  game { leftPlayer = (leftPlayer game) - movementSpeed }
updateGameOnKeyDown 'j' game =
  game { rightPlayer = (rightPlayer game) - movementSpeed }
updateGameOnKeyDown 'k' game =
  game { rightPlayer = (rightPlayer game) + movementSpeed }
updateGameOnKeyDown _ game = game

updatePaddles :: PongGame -> PongGame
updatePaddles game =
  foldl (\game_ char -> updateGameOnKeyDown char game_) game (keysDown game)

haltIfPaused :: Float -> PongGame -> PongGame
haltIfPaused seconds game =
  if (gamePaused game)
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
