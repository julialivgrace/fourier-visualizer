module Main where

import           Arrow
import           Seed
import           Fourier
import           Data.Complex
import           Graphics.Gloss

circleSeed :: Seed
circleSeed = createSeed f n spaceScale timeScale
 where
  f          = cis . (*) (2 * pi)
  n          = 101
  spaceScale = 100
  timeScale  = 1

inits :: Seed
inits = Seed { arrows = arrows, spaceScale = 1, timeScale = 0.01 }
  where arrows = [ (50 :+ 50, s) | s <- [-10 .. 10] ]

main :: IO ()
main = animate FullScreen (greyN 0.05) (drawSeed circleSeed)
