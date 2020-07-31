module Main where

import           Data.Complex
import qualified ExampleSeeds                  as E
import           Graphics.Gloss
import           Seed                           ( drawSeed )

main :: IO ()
main = animate FullScreen (greyN 0.05) (drawSeed E.heartSeed)
