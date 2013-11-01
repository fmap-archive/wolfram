module Data.Automata.Wolfram.Writers.Text (showAutomata) where

import Data.Automata.Wolfram.Types
import Data.Automata.Wolfram.Writers

showAutomata :: Coordinates -> [Universe State] -> String
showAutomata xy = unlines . map show . collapse xy
