{-# LANGUAGE Arrows #-}
module Main where

-- base
import Control.Arrow
import Control.Monad
import Data.Functor
import Data.Maybe

-- essence-of-live-coding
import LiveCoding

-- essence-of-live-coding-gloss
import LiveCoding.Gloss

import Debug.Trace
import Data.Function ((&))

border :: Num a => (a, a)
border = (300, 400)

ballRadius :: Num a => a
ballRadius = 20

glossSettings :: GlossSettings
glossSettings = defaultSettings
  { debugEvents = True
  , displaySetting = InWindow "Essence of Live Coding Tutorial" (border ^* 2) (20, 20)
  }

liveProgram :: LiveProgram (HandlingStateT IO)
liveProgram = liveCell $ glossWrapC glossSettings $ glossCell
  -- & (`withDebuggerC` statePlay) -- Uncomment to display the internal state

glossCell :: Cell PictureM () ()
glossCell = proc () -> do
  events <- constM ask -< ()
  ball <- ballSim -< events
  addPicture -< ballPic ball
  returnA    -< ()

ballSim :: Monad m => Cell m [Event] (Float, Float)
ballSim = feedback ((0, 0), (0, 0)) $ proc (events, (lastBall, lastSpeed)) -> do
  let accMouse = sum2d $ (^-^ lastBall) <$> clicks events
      accCollision = sum2d $ catMaybes
        [ guard (fst lastBall < - fst border + ballRadius && fst lastSpeed < 0) $> (-2 * fst lastSpeed, 0)
        , guard (fst lastBall >   fst border - ballRadius && fst lastSpeed > 0) $> (-2 * fst lastSpeed, 0)
        , guard (snd lastBall < - snd border + ballRadius && snd lastSpeed < 0) $> (0, -2 * snd lastSpeed)
        , guard (snd lastBall >   snd border - ballRadius && snd lastSpeed > 0) $> (0, -2 * snd lastSpeed)
        ]
  let speed = sum2d [accMouse, accCollision ^* 0.97, lastSpeed ^* 0.996]
  ball <- integrate *** integrate -< speed
  returnA -< (ball, (ball, speed))

ballPic :: (Float, Float) -> Picture
ballPic (x, y) = translate x y $ color white $ thickCircle 10 ballRadius

clicks :: [Event] -> [(Float, Float)]
clicks = catMaybes . map click

click :: Event -> Maybe (Float, Float)
click event@(EventKey (MouseButton LeftButton) Down _ pos) = traceShow event $ Just pos
click event = traceShow event $ Nothing

(^-^) :: (Float, Float) -> (Float, Float) -> (Float, Float)
(x1, y1) ^-^ (x2, y2) = (x1 - x2, y1 - y2)

(^+^) :: (Float, Float) -> (Float, Float) -> (Float, Float)
(x1, y1) ^+^ (x2, y2) = (x1 + x2, y1 + y2)

(^*) :: Num a => (a, a) -> a -> (a, a)
(x, y) ^* t = (x * t, y * t)

sum2d :: [(Float, Float)] -> (Float, Float)
sum2d = foldr (^+^) (0, 0)

main :: IO ()
main = runHandlingStateT $ foreground liveProgram
