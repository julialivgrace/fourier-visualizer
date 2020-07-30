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

-- A seed is a list of arrows and their corresponding frequencies, as well as scaling information
data Seed = Seed { arrows :: [(Arrow, Integer)], spaceScale :: Arrow, timeScale :: Float }

createSeed :: (Float -> Complex Float) -> Int -> Arrow -> Float -> Seed
createSeed f n spaceScale timeScale = Seed { arrows     = arrows
                                           , spaceScale = spaceScale
                                           , timeScale  = timeScale
                                           }
 where
  -- arrow frequencies are ordered, in cycles / second, 0, 1, -1, 2, -2, 3, -3, etc.
  freqs  = take n (0 : concat [ [x, -x] | x <- [1 ..] ])
  cs     = map (Fourier.coefficient f) freqs
  arrows = zip cs freqs

arrowsAt :: Seed -> Float -> [Arrow]
arrowsAt seed t = map updateArrow (arrows seed)
 where
  updateArrow (arrow, freq) =
    spaceScale seed * cis (2 * pi * fromInteger freq * t) * arrow

drawSeed :: Seed -> Float -> Picture
drawSeed seed seconds = Pictures [vectors, tip, trail]
 where
  t         = seconds * timeScale seed
  positions = arrange $ arrowsAt seed t
  (x, y)    = last positions
  -- draw stuff
  vectors   = color (light aquamarine) . line $ positions
  tip       = color red . translate x y $ circleSolid 5
  trail     = color white . line $ map (endpoint . arrowsAt seed) ts
  -- trail passes through through the endpoints at t, t - 1/100, t - 2/100, t - 3/100, etc.
  ts        = takeWhile (> 0) . map (\x -> t - x / 100) $ [0 .. 80]
