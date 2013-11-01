module Data.Automata.Wolfram.Writers.Text (showAutomata) where

import Data.Automata.Wolfram.Types

showAutomata :: (Int, Int) -> [Universe State] -> String
showAutomata (x,y) = unlines . map show . take y . map (toList $ x `div` 2)
