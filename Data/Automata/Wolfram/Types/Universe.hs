module Data.Automata.Wolfram.Types.Universe where

import Control.Comonad
import Data.Automata.Wolfram.Types.State
import Data.Automata.Wolfram.Types.Rule

data Universe x = Universe [x] x [x]

instance Functor Universe where
  fmap fn (Universe as b cs) = Universe (map fn as) (fn b) (map fn cs)

instance Comonad Universe where
  extract (Universe _ c _) = c
  duplicate uni  = Universe (tail $ iterate left uni) uni (tail $ iterate right uni)

right :: Universe x -> Universe x
right (Universe as b (c:cs)) = Universe (b:as) c cs

left :: Universe x -> Universe x
left (Universe (a:as) b cs) = Universe as a (b:cs)

toList :: Int -> Universe a -> [a]
toList n (Universe as b cs) = take n as ++ return b ++ take n cs

toUniverse :: [a] -> Universe a
toUniverse xs = Universe begin middle end
  where begin  = take hlen xs
        middle = xs!!hlen
        end    = drop (succ hlen) xs
        hlen   = flip div 2 . length $ xs 

instance (Show a) => Show (Universe a) where
  show (Universe xs y zs) = concat . map show $
    [ xs , return y , zs ]

expand :: Universe State -> Universe State
expand (Universe as b cs) = Universe (as ++ repeat I) b (cs ++ repeat I)
