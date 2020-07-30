module Seed
  ( Seed
  , createSeed
  , arrowsAt
  , drawSeed
  )
where

import           Arrow                          ( Arrow
                                                , arrange
                                                , endpoint
                                                )
import qualified Fourier
import           Data.Complex
import           Graphics.Gloss

-- A seed represents an initial state of arrow frequencies, directions, and magnitudes
-- Any complex function can be traced out by changing this initial state.
-- It also contains some drawing and scaling information.
data Seed = Seed { arrows :: [(Arrow, Integer)], spaceScale :: Float, timeScale :: Float, trailLength :: Float }

createSeed :: (Float -> Complex Float) -> Int -> Float -> Float -> Float -> Seed
createSeed f n spaceScale timeScale trailLength = Seed
  { arrows      = arrows
  , spaceScale  = spaceScale
  , timeScale   = timeScale
  -- measure trail length by percentage of a time-scaled second
  , trailLength = trailLength
  }
 where
  -- order arrow frequencies 0, 1, -1, 2, -2, 3, -3, etc.
  freqs             = take n (0 : concat [ [x, -x] | x <- [1 ..] ])
  startingPositions = map (Fourier.coefficient f) freqs
  arrows            = zip startingPositions freqs

arrowsAt :: Seed -> Float -> [Arrow]
arrowsAt seed t = map updateArrow (arrows seed)
  where updateArrow (arrow, freq) = cis (2 * pi * fromInteger freq * t) * arrow

drawSeed :: Seed -> Float -> Picture
drawSeed seed seconds = scale s s $ Pictures [vectors, tip, trail]
 where
  s         = spaceScale seed
  t         = timeScale seed * seconds
  positions = arrange $ arrowsAt seed t
  (x, y)    = last positions
  -- draw stuff
  vectors   = color (light aquamarine) . line $ positions
  tip       = color red . translate x y $ circleSolid (5 / spaceScale seed)
  trail     = color white . line $ map (endpoint . arrowsAt seed) ts
  -- the trail draws through points with a step size of 1/1000 of a (time-scaled) second
  tf        = floorTo 3 t
  ti        = max 0 (tf - trailLength seed)
  ts        = [ti, ti + 0.001 .. t]

floorTo :: (RealFrac a) => Integer -> a -> a
floorTo n = (/ 10 ^ n) . fromIntegral . floor . (* 10 ^ n)
