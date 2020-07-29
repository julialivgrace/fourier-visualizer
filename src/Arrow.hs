module Arrow where

import Data.Complex
import Graphics.Gloss

type Arrow = Complex Float

-- A seed is a list of arrows and their corresponding rotation speeds
type Seed = [(Arrow, Float)]

rotateArrow :: Arrow -> Float -> Arrow
rotateArrow arrow angle = cis angle * arrow

vectorize :: Arrow -> Vector
vectorize a = (realPart a, imagPart a)

arrowsAt :: Seed -> Float -> [Arrow]
arrowsAt seed t = map (\(arrow, speed) -> rotateArrow arrow (speed * t)) seed

-- Create a path through the arrows arranged tip to tail
arrange :: [Arrow] -> Path
arrange = map vectorize . scanl (+) (0 :+ 0)

endpoint :: Seed -> Float -> Point
endpoint seed = last . arrange . arrowsAt seed

drawArrows :: Seed -> Float -> Picture
drawArrows seed seconds = Pictures [vectors, tip, trail]
  where
    t = seconds / 5
    positions = arrange (arrowsAt seed t)
    (x, y) = endpoint seed t
    -- draw stuff
    vectors = color (light aquamarine) $ line positions
    tip = color red $ translate x y $ circleSolid 5
    trail = color white $ line (map (endpoint seed) . takeWhile (>=0) $ [t, t - 0.01 .. t - 0.05])
