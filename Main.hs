{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

-- base
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Fix (MonadFix)
import Data.Foldable
import Data.Function ((&))
import Data.Functor
import Data.Maybe
import Text.Read (readMaybe)

-- vector-space
import Data.VectorSpace

-- utf8-string
import Data.ByteString.UTF8 hiding (take)

-- essence-of-live-coding
import LiveCoding hiding (integrate)

-- essence-of-live-coding-gloss
import LiveCoding.Gloss

-- essence-of-live-coding-pulse
import LiveCoding.Pulse

-- essence-of-live-coding-warp
import LiveCoding.Warp

-- * Main program

main :: IO ()
main = runHandlingStateT $ foreground liveProgram

-- Uncomment the different *RunCells to start different media backends!
liveProgram :: LiveProgram (HandlingStateT IO)
liveProgram = liveCell $ proc _ -> do
  requestInfoMaybe <- warpRunCell -< ()
  isObsHitMaybe <- glossRunCell   -< requestInfoMaybe
  isObsHit <- keep 0              -< isObsHitMaybe
  pulseRunCell                    -< isObsHit
  returnA                         -< ()

-- * Warp subcomponent

-- | Starts a webserver on port 8080
warpRunCell :: Cell (HandlingStateT IO) () (Maybe RequestInfo)
warpRunCell = runWarpC 8080 warpCell

-- | This handles the incoming request from the webserver
warpCell :: Cell IO ((), Request) (RequestInfo, Response)
warpCell = proc ((), request) -> do
  body <- arrM lazyRequestBody -< request
  returnA -< (getRequestInfo request, emptyResponse)

-- | The type of interesting data from the request
type RequestInfo = (String, Maybe (Float, Float))

-- | Extract data from the request to use in the rest of the program
getRequestInfo :: Request -> RequestInfo
getRequestInfo = getQueryString &&& parseWarpImpulse

getQueryString :: Request -> String
getQueryString = toString . renderQuery False . queryString

parseWarpImpulse :: Request -> Maybe (Float, Float)
parseWarpImpulse request = do
  let query = queryString request
  xText <- join $ lookup "x" query
  yText <- join $ lookup "y" query
  x <- readMaybe $ toString xText
  y <- readMaybe $ toString yText
  return (x, y)

keepNStrings :: (Monad m, Data a) => Int -> Cell m (Maybe a) [a]
keepNStrings n = foldC step []
  where
    step stringMaybe strings = take n $ maybe id (:) stringMaybe strings

-- Extend this for a more interesting website
emptyResponse :: Response
emptyResponse = responseLBS
  status200
  [("Content-Type", "text/plain")]
  "Yep, it's working"

-- * Gloss subcomponent

-- ** Backend setup

borderX :: Num a => a
borderX = 300

borderY :: Num a => a
borderY = 400

border :: Num a => (a, a)
border = (borderX, borderY)

glossSettings :: GlossSettings
glossSettings = defaultSettings
  { debugEvents = True
  , displaySetting = InWindow "Essence of Live Coding Tutorial" (border ^* 2) (0, 0)
  }

-- | Run the gloss backend at 30 frames per second
glossRunCell :: Cell (HandlingStateT IO) (Maybe RequestInfo) (Maybe Float)
glossRunCell = glossWrapC glossSettings $ bufferedGlossCell
  & (`withDebuggerC` statePlay) -- Uncomment to display the internal state

bufferedGlossCell :: Cell PictureM (Maybe RequestInfo) Float
bufferedGlossCell = feedback False $ proc (requestInfoMaybe, requestCame) -> do
  requestInfoMaybe' <- buffer -< [Pop | requestCame] ++ maybePush requestInfoMaybe
  glossCell -< requestInfoMaybe'

-- ** Main gloss cell

-- | This cell is called for every frame of the graphics output
glossCell :: Cell PictureM (Maybe RequestInfo) (Float, Bool)
glossCell = proc requestInfoMaybe -> do
  let warpImpulse = snd =<< requestInfoMaybe
  events <- constM ask        -< ()
  (ball, isObsHit) <- ballSim -< (events, warpImpulse)
  addPicture                  -< holePic hole
  addPicture                  -< pictures $ obstaclePic <$> obstacles
  addPicture                  -< ballPic ball
  actOnRequest                -< requestInfoMaybe
  returnA                     -< (isObsHit, isJust requestInfoMaybe)

-- * Parse web request

actOnRequest :: Cell PictureM (Maybe RequestInfo) ()
actOnRequest = proc requestInfoMaybe -> do
  string <- keep "" -< fst <$> requestInfoMaybe
  addPicture
    -< translate (-250) 300
    $  scale 0.2 0.2
    $  color red
    $  text string

-- ** Playing field

data Hole = Hole
  { holePos :: (Float, Float)
  , holeRad :: Float
  }

hole :: Hole
hole = Hole { holePos = (0, 250), holeRad = 40 }

data Obstacle = Obstacle
  { obstaclePos :: (Float, Float)
  , obstacleRad :: Float
  , obstacleRep :: Float
  }

obstacles :: [Obstacle]
obstacles =
  [ Obstacle
    { obstaclePos = (-200, -200)
    , obstacleRad = 50
    , obstacleRep = 5
    }
  , Obstacle
    { obstaclePos = (0, 150)
    , obstacleRad = 30
    , obstacleRep = 10
    }
  , Obstacle
    { obstaclePos = (100, 150)
    , obstacleRad = 30
    , obstacleRep = 30
    }
  , Obstacle
    { obstaclePos = (-100, 150)
    , obstacleRad = 30
    , obstacleRep = 20
    }
  ]

obstaclePic :: Obstacle -> Picture
obstaclePic Obstacle { obstaclePos = (x, y), .. }
  = translate x y
  $ color (withRed (obstacleRep / 50) blue)
  $ thickCircle (obstacleRad / 2) obstacleRad

-- ** Ball

ballRadius :: Num a => a
ballRadius = 20

-- | Draw the ball in gloss
ballPic :: Ball -> Picture
ballPic Ball { ballPos = (x, y) } = translate x y $ color white $ thickCircle (ballRadius / 2) ballRadius

holePic :: Hole -> Picture
holePic Hole { holePos = (x, y), holeRad } = translate x y $ color green $ thickCircle (holeRad / 2) holeRad

-- | The type of internal state of the 'ballSim'
data Ball = Ball
  { ballPos :: (Float, Float)
  , ballVel :: (Float, Float)
  } deriving Data

ballPosX = fst . ballPos
ballPosY = snd . ballPos
ballVelX = fst . ballVel
ballVelY = snd . ballVel

repulse :: Ball -> Obstacle -> (Float, Float)
repulse Ball { .. } Obstacle { .. }
  = let vecDiff = ballPos ^-^ obstaclePos
    in  if magnitude vecDiff < ballRadius + obstacleRad
      then obstacleRep *^ vecDiff
      else zeroV

-- | Simulate the position of the ball, given recent events such as mouse clicks
ballSim :: (Monad m, MonadFix m) => Cell m ([Event], Maybe (Float, Float)) (Ball, Float)
ballSim = proc (events, webImpulse) -> do
  rec
    let accMouse = sumV $ (^-^ ballPos ball) <$> clicks events
        accCollision = sumV $ catMaybes
          [ guard (ballPosX ball < - borderX + ballRadius && ballVelX ball < 0)
              $> (-2 * ballVelX ball, 0)
          , guard (ballPosX ball >   borderX - ballRadius && ballVelX ball > 0)
              $> (-2 * ballVelX ball, 0)
          , guard (ballPosY ball < - borderY + ballRadius && ballVelY ball < 0)
              $> (0, -2 * ballVelY ball)
          , guard (ballPosY ball >   borderY - ballRadius && ballVelY ball > 0)
              $> (0, -2 * ballVelY ball)
          ]
        frictionCoeff
          | magnitude (ballVel ball) < 30 = -4
          | magnitude (ballPos ball ^-^ holePos hole) < holeRad hole - ballRadius = -20
          | otherwise = -0.3
        repulsion = sumV $ repulse ball <$> obstacles
        isObsHit = atan $ 0.04 * magnitude repulsion
    frictionVel <- integrate -< frictionCoeff *^ ballVel ball ^+^ repulsion
    impulses <- sumS -< sumV [accMouse, 0.97 *^ accCollision, fromMaybe (0, 0) webImpulse]
    let newVel = frictionVel ^+^ impulses
    newPos <- integrate -< newVel
    let ball = Ball newPos newVel
  returnA -< (ball, isObsHit)

-- | Extract the positions of left mouse clicks
clicks :: [Event] -> [(Float, Float)]
clicks = mapMaybe click

click :: Event -> Maybe (Float, Float)
click (EventKey (MouseButton LeftButton) Down _ pos) = Just pos
click _ = Nothing

-- * Pulse subcomponent

-- | Run the PulseAudio backend at 48000 samples per second
pulseRunCell :: Cell (HandlingStateT IO) Float [()]
pulseRunCell = pulseWrapC 1600 pulseCell

pulseCell :: Monad m => PulseCell m Float ()
pulseCell = proc b -> do
  pulse <- sawtooth -< 440 + 20 * b
  addSample         -< pulse * b

-- * Utilities

sumS
  :: (Monad m, Data v, VectorSpace v)
  => Cell m v v
sumS = foldC (^+^) zeroV

integrate
  :: (Monad m, Data v, VectorSpace v, Fractional (Scalar v))
  => Cell m v v
integrate = arr (^/ fromIntegral (stepsPerSecond glossSettings)) >>> sumS
