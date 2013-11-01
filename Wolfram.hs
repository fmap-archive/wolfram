import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Control.Comonad

type Rule = [State]
data State = A | I
data Universe x = Universe [x] x [x]

neighbours :: Universe x -> (x,x,x)
neighbours (Universe (a:as) b (c:cs)) = (a,b,c)

right :: Universe x -> Universe x
right (Universe as b (c:cs)) = Universe (b:as) c cs

left :: Universe x -> Universe x
left (Universe (a:as) b cs) = Universe as a (b:cs)

instance Functor Universe where
  fmap fn (Universe as b cs) = Universe (map fn as) (fn b) (map fn cs)

instance Comonad Universe where
  extract (Universe _ c _) = c
  duplicate uni  = Universe (tail $ iterate left uni) uni (tail $ iterate right uni)

expand :: Universe State -> Universe State
expand (Universe as b cs) = Universe (as ++ repeat I) b (cs ++ repeat I)

applyRule :: Rule -> Universe State -> State
applyRule r u = (r!!) $ case (neighbours u) of
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

toUniverse :: [a] -> Universe a
toUniverse xs = Universe begin middle end
  where begin  = take hlen xs
        middle = xs!!hlen
        end    = drop (succ hlen) xs
        hlen   = flip div 2 . length $ xs 

toList :: Int -> Universe a -> [a]
toList n (Universe as b cs) = take n as ++ return b ++ take n cs

pad :: Int -> a -> [a] -> [a]
pad len with list 
  | len == length list = list
  | otherwise = with : pad (pred len) with list

binary :: Int -> String
binary n = showIntAtBase 2 intToDigit n []

intToRule :: Int -> Rule
intToRule = map toState . pad 8 '0' . binary

class Stateable a where
  toState :: a -> State

instance Stateable Char where
  toState '0' = I
  toState '1' = A

instance Stateable Int where
  toState = toState . intToDigit

instance (Show a) => Show (Universe a) where
  show (Universe xs y zs) = concat . map show $
    [ xs 
    , return y 
    , zs
    ]

instance Show State where
  show A = "■"
  show I = " "
  showList = showString . concat . map show

runner :: Int -> String -> (Int, Int) -> IO ()
runner rule initial (x,y) = do
  putStrLn $ "\n\nRule " ++ show rule ++ ":\n"
  let universe  = expand $ toUniverse $ map toState initial
  let universes = automata rule universe
  mapM_ print $ take y . map (toList $ x `div` 2) $ universes
  
main :: IO ()
main = foldr (>>) epsilon $ map (\n -> runner n starter dimensions) [0..255]
  where epsilon = return ()
        starter = "00010011011111000000000000010100101010"
        dimensions = (60,30)
