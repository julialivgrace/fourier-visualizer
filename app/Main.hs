module Main where

import           Seed                           ( Seed(..)
                                                , createSeed
                                                , drawSeed
                                                )
import           Data.Complex
import           Graphics.Gloss

circleSeed :: Seed
circleSeed = createSeed f n spaceScale timeScale
 where
  f          = cis . (*) (2 * pi)
  n          = 101
  spaceScale = 100
  timeScale  = 1

lineSeed :: Seed
lineSeed = createSeed f n spaceScale timeScale
 where
  f t = (cos (2 * pi * t) :+ 0) * (1 :+ 1)
  n          = 101
  spaceScale = 100
  timeScale  = 0.5

rodFunction :: Float -> Complex Float
rodFunction x | x < 0.5   = -1
              | x > 0.5   = 1
              | otherwise = 0.5

rodSeed :: Seed
rodSeed = createSeed rodFunction n spaceScale timeScale
 where
  n          = 20
  spaceScale = 500
  timeScale  = 0.1


main :: IO ()
main = animate FullScreen (greyN 0.05) (drawSeed rodSeed)
