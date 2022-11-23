module HW3.Utils where

import HW3.Base
import Data.Ratio

ratToInt :: Integral i => Rational -> Maybe i
ratToInt rat = case divMod (numerator rat) (denominator rat) of
    (num,0) -> Just $ fromIntegral num
    _       -> Nothing