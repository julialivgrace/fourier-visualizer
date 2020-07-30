module ExampleSeeds where

import           Data.Complex
import           Seed                           ( Seed
                                                , createSeed
                                                )

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
              | otherwise = 0

rodSeed :: Seed
rodSeed = createSeed rodFunction n spaceScale timeScale
 where
  n          = 501
  spaceScale = 500
  timeScale  = 0.1

cardioid :: (RealFloat a) => a -> a -> Complex a
cardioid a t = x :+ y
 where
  x = 2 * a * (1 - cos t) * cos t
  y = 2 * a * (1 - cos t) * sin t

cardioidSeed :: Seed
cardioidSeed = createSeed (cardioid 100 . (*) (2 * pi)) n spaceScale timeScale
 where
  n          = 20
  spaceScale = 1
  timeScale  = 0.1

-- www.idius.net/ppp
ppp :: (RealFloat a) => a -> Complex a
ppp t = x (-3.526 + 49.526 * t) :+ y (-3.526 + 49.256 * t)
 where
  x t =
    1.1
      * cos (2 * pi * 1.1 * t)
      + 0.8
      * sin (2 * pi * 2.2 * t + 1.2 * exp (0.008 * t))
      * exp (-0.012 * t)
  y t =
    1.1
      * sin (2 * pi * 1.1 * t)
      + 0.8
      * cos (2 * pi * 2.2 * t + 1.2 * exp (0.008 * t))
      * exp (-0.012 * t)

pppSeed :: Seed
pppSeed = createSeed ppp n spaceScale timeScale
 where
  n          = 20
  spaceScale = 100
  timeScale  = 0.1
