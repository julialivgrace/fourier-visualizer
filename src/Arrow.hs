module Arrow where

import Data.Complex
import Graphics.Gloss

type Arrow = Complex Float

-- A seed is a list of arrows and their corresponding frequencies, as well as scaling information
data Seed = Seed { arrows :: [(Arrow, Integer)], spaceScale :: Arrow, timeScale :: Float }

rotation :: Float -> Arrow
rotation = cis

vectorize :: Arrow -> Vector
vectorize a = (realPart a, imagPart a)

arrowsAt :: Seed -> Float -> [Arrow]
arrowsAt seed t = map updateArrow (arrows seed)
  where
    updateArrow (arrow, freq) = spaceScale seed * rotation (2 * pi * fromInteger freq * timeScale seed * t) * arrow

-- Create a path through the arrows arranged tip to tail
arrange :: [Arrow] -> Path
arrange = map vectorize . scanl (+) (0 :+ 0)

endpoint :: Seed -> Float -> Point
endpoint seed = last . arrange . arrowsAt seed

drawSeed :: Seed -> Float -> Picture
drawSeed seed t = Pictures [vectors, tip, trail]
  where
    positions = arrange . arrowsAt seed $ t
    (x, y) = endpoint seed t
    -- draw stuff
    vectors = color (light aquamarine) $ line positions
    tip = color red $ translate x y $ circleSolid 5
    -- draw path through through the endpoints at t, t - 1/100, t - 2/100, t - 3/100, etc.
    ts = takeWhile (>=0) . map (\x -> t - x/100) $ [0..80]
    trail = color white $ line (map (endpoint seed) ts)
