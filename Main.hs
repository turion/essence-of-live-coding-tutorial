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
import Control.Monad.Fix (MonadFix)
import Data.Function ((&))
import Data.Functor
import Data.Maybe

-- bytestring
import qualified Data.ByteString as ByteString

-- utf8-string
import Data.ByteString.UTF8 (toString)

-- vector-space
import Data.VectorSpace

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

liveProgram :: LiveProgram (HandlingStateT IO)
liveProgram = liveCell $ proc _ -> do
  queryMaybe <- warpRunCell -< ()
  glossRunCell              -< queryMaybe
  -- pulseRunCell              -< ()
  returnA                   -< ()

-- * Warp subcomponent

warpRunCell :: Cell (HandlingStateT IO) () (Maybe Query)
warpRunCell = runWarpC 8080 warpCell

warpCell :: Cell IO ((), Request) (Query, Response)
warpCell = proc ((), request) -> do
  body <- arrM lazyRequestBody -< request
  returnA -< (queryString request, emptyResponse)

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
  , displaySetting = InWindow "Essence of Live Coding Tutorial" (border ^* 2) (20, 20)
  }

glossRunCell :: Cell (HandlingStateT IO) (Maybe Query) (Maybe ())
glossRunCell = glossWrapC glossSettings $ glossCell
  & (`withDebuggerC` statePlay) -- Uncomment to display the internal state

-- ** Main gloss cell

glossCell :: Cell PictureM (Maybe Query) ()
glossCell = proc queryMaybe -> do
  events <- constM ask -< ()
  ball <- ballSim      -< events
  addPicture           -< holePic hole
  addPicture           -< pictures $ obstaclePic <$> obstacles
  addPicture           -< ballPic ball
  actOnRequest         -< queryMaybe
  returnA              -< ()

-- * Parse web request

actOnRequest :: Cell PictureM (Maybe Query) ()
actOnRequest = proc queryMaybe -> do
  string <- keep "" -< toString <$> renderQuery False <$> queryMaybe
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

ballPic :: Ball -> Picture
ballPic Ball { ballPos = (x, y) } = translate x y $ color white $ thickCircle (ballRadius / 2) ballRadius

holePic :: Hole -> Picture
holePic Hole { holePos = (x, y), holeRad } = translate x y $ color green $ thickCircle (holeRad / 2) holeRad

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

ballSim :: (Monad m, MonadFix m) => Cell m [Event] Ball
ballSim = proc events -> do
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
    frictionVel <- integrate -< frictionCoeff *^ ballVel ball ^+^ repulsion
    impulses <- sumS -< sumV [accMouse, 0.97 *^ accCollision]
    let newVel = frictionVel ^+^ impulses
    newPos <- integrate -< newVel
    let ball = Ball newPos newVel
  returnA -< ball

clicks :: [Event] -> [(Float, Float)]
clicks = catMaybes . map click

click :: Event -> Maybe (Float, Float)
click (EventKey (MouseButton LeftButton) Down _ pos) = Just pos
click _ = Nothing

-- * Pulse subcomponent

pulseRunCell :: Cell (HandlingStateT IO) () [()]
pulseRunCell = pulseWrapC 1600 $ arr (const 440) >>> sawtooth >>> addSample

-- * Utilities

sumS
  :: (Monad m, Data v, VectorSpace v)
  => Cell m v v
sumS = foldC (^+^) zeroV

integrate
  :: (Monad m, Data v, VectorSpace v, Fractional (Scalar v))
  => Cell m v v
integrate = arr (^/ fromIntegral (stepsPerSecond glossSettings)) >>> sumS
