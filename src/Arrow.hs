module Arrow where

import Data.Complex
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as A

data Arrow = Arrow {speed :: Integer, initialPosition :: Vector}

inits :: [Arrow]
inits = [Arrow s (50, 50) | s <- [-10 .. 10]]

vectorize :: Complex Float -> Vector
vectorize c = (realPart c, imagPart c)

positionAt :: Float -> Arrow -> Vector
positionAt t arrow = rotateV angle . initialPosition $ arrow
  where
    angle = fromInteger (speed arrow) * t

arrowPath :: Float -> [Arrow] -> Path
arrowPath t = scanl (A.+) (0, 0) . map (positionAt t)

trailPath :: [Float] -> [Arrow] -> Path
trailPath ts inits = map endpointAt ts
  where
    endpointAt t = last (arrowPath t inits)

drawArrows :: [Arrow] -> Float -> Picture
drawArrows inits seconds = Pictures [vectors, tip, trail]
  where
    t = seconds / 5
    positions = arrowPath t inits
    (x, y) = last positions
    -- draw stuff
    vectors = color (light aquamarine) $ line positions
    trail = color white $ line (trailPath [0, 0.01 .. t] inits)
    tip = color red $ translate x y $ circleSolid 5
