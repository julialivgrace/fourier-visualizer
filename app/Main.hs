module Main where

import Arrow
import Data.Complex
import Graphics.Gloss

inits :: Seed
inits = [(100 :+ 50, s) | s <- [-10 .. 10]]

main :: IO ()
main = animate FullScreen (greyN 0.05) (drawArrows inits)
