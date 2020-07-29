module Main where

import Arrow
import Fourier
import Data.Complex
import Graphics.Gloss

circleSeed :: Seed
circleSeed = Seed {arrows = arrows, spaceScale = 100, timeScale = 1}
    where
        f = cis . (*) (2*pi)
        frequencies = [-50 .. 50]
        coefficients = map (coefficient f) frequencies
        arrows = zip coefficients frequencies

inits :: Seed
inits = Seed {arrows = arrows, spaceScale = 1, timeScale = 0.01}
    where 
        arrows = [(50 :+ 50, s) | s <- [-10 .. 10]]

main :: IO ()
main = animate FullScreen (greyN 0.05) (drawSeed circleSeed)
