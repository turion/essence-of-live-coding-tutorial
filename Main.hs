{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Data.ByteString.UTF8

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
  -- warpRunCell  -< ()
  glossRunCell -< ()
  -- pulseRunCell -< ()
  returnA      -< ()

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
type RequestInfo = Query

-- | Extract data from the request to use in the rest of the program
getRequestInfo :: Request -> RequestInfo
getRequestInfo = queryString

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
glossRunCell :: Cell (HandlingStateT IO) () (Maybe ())
glossRunCell = glossWrapC glossSettings $ glossCell
  -- & (`withDebuggerC` statePlay) -- Uncomment to display the internal state

-- ** Main gloss cell

-- | This cell is called for every frame of the graphics output
glossCell :: Cell PictureM () ()
glossCell = proc () -> do
  events <- constM ask -< ()
  ball <- ballSim -< events
  addPicture -< ballPic ball
  returnA    -< ()

-- ** Ball

ballRadius :: Num a => a
ballRadius = 20

-- | Draw the ball in gloss
ballPic :: Ball -> Picture
ballPic Ball { ballPos = (x, y) } = translate x y $ color white $ thickCircle (ballRadius / 2) ballRadius

-- | The type of internal state of the 'ballSim'
data Ball = Ball
  { ballPos :: (Float, Float)
  , ballVel :: (Float, Float)
  } deriving Data

ballPosX = fst . ballPos
ballPosY = snd . ballPos
ballVelX = fst . ballVel
ballVelY = snd . ballVel

-- | Simulate the position of the ball, given recent events such as mouse clicks
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
    frictionVel <- integrate -< (-0.3) *^ ballVel ball
    impulses <- sumS -< sumV [accMouse, 0.97 *^ accCollision]
    let newVel = frictionVel ^+^ impulses
    newPos <- integrate -< newVel
    let ball = Ball newPos newVel
  returnA -< ball

-- | Extract the positions of left mouse clicks
clicks :: [Event] -> [(Float, Float)]
clicks = mapMaybe click

click :: Event -> Maybe (Float, Float)
click (EventKey (MouseButton LeftButton) Down _ pos) = Just pos
click _ = Nothing

-- * Pulse subcomponent

-- | Run the PulseAudio backend at 48000 samples per second
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
