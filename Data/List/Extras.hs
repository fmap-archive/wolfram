module Data.List.Extras (pad) where

pad :: Int -> a -> [a] -> [a]
pad len with list 
  | len == length list = list
  | otherwise = with : pad (pred len) with list
