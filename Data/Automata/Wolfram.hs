module Data.Automata.Wolfram (automata) where

import Control.Comonad
import Data.Automata.Wolfram.Types
import Data.List.Extras (pad)
import Data.String.Extras (intToBinary)

neighbours :: Universe x -> (x,x,x)
neighbours (Universe (a:_) b (c:_)) = (a,b,c)

applyRule :: Rule -> Universe State -> State
applyRule r u = (r!!) $ case neighbours u of
  (A,A,A) -> 0
  (A,A,I) -> 1
  (A,I,A) -> 2
  (A,I,I) -> 3
  (I,A,A) -> 4
  (I,A,I) -> 5
  (I,I,A) -> 6
  (I,I,I) -> 7

automata :: Int -> Universe State -> [Universe State]
automata = iterate . transition . intToRule

transition :: Rule -> Universe State -> Universe State
transition rule = (=>> applyRule rule) 

intToRule :: Int -> Rule
intToRule = map toState . pad (2^3) '0' . intToBinary
