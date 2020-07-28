module Main where

import Arrow
import Graphics.Gloss

main :: IO ()
main = animate FullScreen (greyN 0.05) (drawArrows inits)
