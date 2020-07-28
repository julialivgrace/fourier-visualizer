module Arrow where

import Data.Complex
import Graphics.Gloss

type Arrow = Complex Float

-- A seed is a list of rotation speeds and their corresponding arrow's starting positions
type Seed = [(Arrow, Float)]

rotateArrow :: Arrow -> Float -> Arrow
rotateArrow arrow angle = cis angle * arrow

vectorize :: Arrow -> Vector
vectorize a = (realPart a, imagPart a)

updateSeed :: Seed -> Float -> [Arrow]
updateSeed seed t = map (\(arrow, speed) -> rotateArrow arrow (speed * t)) seed

-- Create a path through the arrows arranged tip to tail
arrange :: [Arrow] -> Path
arrange = map vectorize . scanl (+) (0 :+ 0)

arrowsAt :: Seed -> Float -> Path
arrowsAt seed t = arrange (updateSeed seed t)

endpoint :: Seed -> Float -> Vector
endpoint seed t = last (arrowsAt seed t)

drawArrows :: Seed -> Float -> Picture
drawArrows seed seconds = Pictures [vectors, tip, trail]
  where
    t = seconds / 5
    positions = arrowsAt seed t
    (x, y) = endpoint seed t
    -- draw stuff
    vectors = color (light aquamarine) $ line positions
    trail = color white $ line (map (endpoint seed) [0, 0.01 .. t])
    tip = color red $ translate x y $ circleSolid 5
