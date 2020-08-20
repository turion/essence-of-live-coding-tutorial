{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

-- base
import Control.Arrow
import Control.Monad
import Control.Monad.Fix (MonadFix)
import Data.Function ((&))
import Data.Functor
import Data.Maybe

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
  -- warpRunCell  -< ()
  glossRunCell -< ()
  -- pulseRunCell -< ()
  returnA      -< ()

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

glossRunCell :: Cell (HandlingStateT IO) () ()
glossRunCell = glossWrapC glossSettings $ glossCell
  -- & (`withDebuggerC` statePlay) -- Uncomment to display the internal state

-- ** Main gloss cell

glossCell :: Cell PictureM () ()
glossCell = proc () -> do
  events <- constM ask -< ()
  ball <- ballSim -< events
  addPicture -< ballPic ball
  returnA    -< ()

-- ** Ball

ballRadius :: Num a => a
ballRadius = 20

ballPic :: Ball -> Picture
ballPic Ball { ballPos = (x, y) } = translate x y $ color white $ thickCircle (ballRadius / 2) ballRadius

data Ball = Ball
  { ballPos :: (Float, Float)
  , ballVel :: (Float, Float)
  } deriving Data

ballPosX = fst . ballPos
ballPosY = snd . ballPos
ballVelX = fst . ballVel
ballVelY = snd . ballVel

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

clicks :: [Event] -> [(Float, Float)]
clicks = catMaybes . map click

click :: Event -> Maybe (Float, Float)
click (EventKey (MouseButton LeftButton) Down _ pos) = Just pos
click _ = Nothing

-- * Pulse subcomponent

pulseRunCell :: Cell (HandlingStateT IO) () [()]
pulseRunCell = pulseWrapC 1600 $ oscAt 440 >>> addSample

-- * Utilities

sumS
  :: (Monad m, Data v, VectorSpace v)
  => Cell m v v
sumS = Cell
  { cellState = zeroV
  , cellStep = \accum v -> return (accum, accum ^+^ v)
  }

integrate
  :: (Monad m, Data v, VectorSpace v, Fractional (Scalar v))
  => Cell m v v
integrate = arr (^/ fromIntegral (stepsPerSecond glossSettings)) >>> sumS
