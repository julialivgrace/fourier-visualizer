module ExampleSeeds where

import           Data.Complex
import           Seed                           ( Seed
                                                , createSeed
                                                )

circleSeed :: Seed
circleSeed = createSeed f n spaceScale timeScale trailLength
 where
  f           = cis . (*) (2 * pi)
  n           = 101
  spaceScale  = 300
  timeScale   = 1
  trailLength = 0.8

lineSeed :: Seed
lineSeed = createSeed f n spaceScale timeScale trailLength
 where
  f t = (cos (2 * pi * t) :+ 0) * (1 :+ 1)
  n           = 101
  spaceScale  = 300
  timeScale   = 0.5
  trailLength = 1

rodFunction :: Float -> Complex Float
rodFunction x | x < 0.5   = -1
              | x > 0.5   = 1
              | otherwise = 0

rodSeed :: Seed
rodSeed = createSeed rodFunction n spaceScale timeScale trailLength
 where
  n           = 501
  spaceScale  = 500
  timeScale   = 0.1
  trailLength = 0.01

cardioid :: (RealFloat a) => a -> a -> Complex a
cardioid a t = x :+ y
 where
  x = 2 * a * (1 - cos t) * cos t
  y = 2 * a * (1 - cos t) * sin t

cardioidSeed :: Seed
cardioidSeed = createSeed (cardioid 100 . (*) (2 * pi))
                          n
                          spaceScale
                          timeScale
                          trailLength
 where
  n           = 21
  spaceScale  = 1
  timeScale   = 0.1
  trailLength = 0.8

lissajous :: (RealFloat a) => a -> a -> a -> Complex a
lissajous a b t = cos (a * t) :+ sin (b * t)

infinity :: (RealFloat a) => a -> Complex a
infinity = lissajous 1 2

infinitySeed :: Seed
infinitySeed = createSeed (infinity . (*) (2 * pi))
                          n
                          spaceScale
                          timeScale
                          trailLength
 where
  n           = 51
  spaceScale  = 300
  timeScale   = 0.1
  trailLength = 0.9
