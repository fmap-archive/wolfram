module Data.String.Extras (intToBinary) where

import Numeric (showIntAtBase)
import Data.Char (intToDigit)

intToBinary :: Int -> String
intToBinary n = showIntAtBase 2 intToDigit n []
