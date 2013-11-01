{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Automata.Wolfram.Writers.Diagram (showAutomata) where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Data.Automata.Wolfram.Types
import Data.Automata.Wolfram.Writers

toSquare s = unitSquare # color s
  where color A = fc black
        color I = id -- transparent

showAutomata :: (Int, Int) -> [Universe State] -> Diagram SVG R2
showAutomata xy = cat unitY .  reverse . map (hcat . map toSquare) . collapse xy
