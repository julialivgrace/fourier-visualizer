module Arrow where

import Data.Complex
import Graphics.Gloss

type Arrow = Complex Float

-- A seed is a list of rotation speeds and their corresponding arrow's starting positions
type Seed = [(Float, Arrow)]

rotateArrow :: Float -> Arrow -> Arrow
rotateArrow angle = (*) (cis angle)

vectorize :: Arrow -> Vector
vectorize a = (realPart a, imagPart a)

updateArrow :: Float -> Float -> Arrow -> Arrow
updateArrow t speed = rotateArrow (t * speed)

updateSeed :: Float -> Seed -> [Arrow]
updateSeed t = map (uncurry $ updateArrow t)

-- Create a path through the arrows arranged tip to tail
arrange :: [Arrow] -> Path
arrange = map vectorize . scanl (+) (0 :+ 0)

arrowsAt :: Float -> Seed -> Path
arrowsAt t = arrange . updateSeed t

-- Create a path through the arranged arrows' endpoints at the given times
trace :: [Float] -> Seed -> Path
trace ts seed = map endpoint ts
  where
    endpoint t = last (arrowsAt t seed)

drawArrows :: Seed -> Float -> Picture
drawArrows seed seconds = Pictures [vectors, tip, trail]
  where
    t = seconds / 5
    positions = arrowsAt t seed
    (x, y) = last positions
    -- draw stuff
    vectors = color (light aquamarine) $ line positions
    trail = color white $ line (trace [0, 0.01 .. t] seed)
    tip = color red $ translate x y $ circleSolid 5
