module Data.Automata.Wolfram.Types.State where

import Data.Char (intToDigit)

data State = A | I

class Stateable a where
  toState :: a -> State

instance Stateable Char where
  toState '0' = I
  toState '1' = A

instance Stateable Int where
  toState = toState . intToDigit

instance Show State where
  show A = "■"
  show I = " "
  showList = showString . concat . map show
