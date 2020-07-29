module Fourier where

import Data.Complex
import Data.Complex.Integrate

coefficient :: (RealFloat a, Integral b) => (a -> Complex a) -> b -> Complex a
coefficient = coefficientWithPrecision 1000

coefficientWithPrecision :: (RealFloat a, Integral b) => Integer -> (a -> Complex a) -> b -> Complex a
coefficientWithPrecision prec f n = integrate g prec 0 1
  where
    term t = f t * cis ((-2) * pi * fromIntegral n * t)
    -- "integrate" requires a function with a complex input, but "term" takes a real input
    g = term . realPart