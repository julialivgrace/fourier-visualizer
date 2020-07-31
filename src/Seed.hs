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
                                                , updateArrow
                                                )
import qualified Fourier
import           Data.Complex
import           Graphics.Gloss

-- A seed represents an initial state of arrow frequencies, directions, and magnitudes
-- Any complex function can be traced out by changing this initial state.
-- It also contains some drawing and scaling information.
data Seed = Seed { _arrows :: [(Arrow, Integer)], _spaceScale :: Float, _timeScale :: Float, _trailLength :: Float }

createSeed :: (Float -> Complex Float) -> Int -> Float -> Float -> Float -> Seed
createSeed f n spaceScale timeScale trailLength = Seed
  { _arrows      = arrows
  , _spaceScale  = spaceScale
  , _timeScale   = timeScale
  -- measure trail length by percentage of a time-scaled second
  , _trailLength = trailLength
  }
 where
  -- order arrow frequencies 0, 1, -1, 2, -2, 3, -3, etc.
  freqs             = take n (0 : concat [ [x, -x] | x <- [1 ..] ])
  startingPositions = map (Fourier.coefficient f) freqs
  arrows            = zip startingPositions freqs

arrowsAt :: Seed -> Float -> [Arrow]
arrowsAt seed t = map (\(arr, freq) -> updateArrow arr freq t) (_arrows seed)

drawSeed :: Seed -> Float -> Picture
drawSeed seed seconds = scale s s $ Pictures [vectors, tip, trail]
 where
  s         = _spaceScale seed
  t         = _timeScale seed * seconds
  positions = arrange $ arrowsAt seed t
  (x, y)    = last positions
  -- draw stuff
  vectors   = color (light aquamarine) . line $ positions
  tip       = color red . translate x y $ circleSolid (5 / _spaceScale seed)
  trail     = color white . line $ map (endpoint . arrowsAt seed) ts
  -- the trail draws through points with a step size of 1/1000 of a (time-scaled) second
  tf        = truncateTo 3 t
  ti        = max 0 (tf - _trailLength seed)
  ts        = [ti, ti + 0.001 .. t]

-- truncate a real number to the given number of decimal places
truncateTo :: (RealFrac a) => Integer -> a -> a
truncateTo n = (/ 10 ^ n) . fromIntegral . truncate . (* 10 ^ n)
