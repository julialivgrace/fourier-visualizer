module Seed where

import           Arrow
import           Fourier
import           Graphics.Gloss
import           Data.Complex

-- A seed is a list of arrows and their corresponding frequencies, as well as scaling information
data Seed = Seed { arrows :: [(Arrow, Integer)], spaceScale :: Arrow, timeScale :: Float }

createSeed :: (Float -> Complex Float) -> Int -> Arrow -> Float -> Seed
createSeed f n spaceScale timeScale = Seed { arrows     = arrows
                                           , spaceScale = spaceScale
                                           , timeScale  = timeScale
                                           }
 where
  freqs  = take n (0 : concat [ [x, -x] | x <- [1 ..] ])
  cs     = map (coefficient f) freqs
  arrows = zip cs freqs

arrowsAt :: Seed -> Float -> [Arrow]
arrowsAt seed t = map updateArrow (arrows seed)
 where
  updateArrow (arrow, freq) =
    spaceScale seed
      * cis (2 * pi * fromInteger freq * timeScale seed * t)
      * arrow

drawSeed :: Seed -> Float -> Picture
drawSeed seed t = Pictures [vectors, tip, trail]
 where
  positions = arrange $ arrowsAt seed t
  (x, y)    = last positions
  -- draw stuff
  vectors   = color (light aquamarine) . line $ positions
  tip       = color red . translate x y $ circleSolid 5
  -- draw path through through the endpoints at t, t - 1/100, t - 2/100, t - 3/100, etc.
  ts        = takeWhile (>= 0) . map (\x -> t - x / 100) $ [0 .. 80]
  trail     = color white . line $ map (endpoint . arrowsAt seed) ts
