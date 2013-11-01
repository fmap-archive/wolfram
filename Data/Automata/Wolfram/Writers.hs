module Data.Automata.Wolfram.Writers where

import Data.Automata.Wolfram.Types

type Coordinates = (Int, Int)

collapse :: Coordinates -> [Universe State] -> [[State]]
collapse (x,y) = take y . map (toList $ x `div` 2)
