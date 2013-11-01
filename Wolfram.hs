import Numeric (showIntAtBase)
import Data.Char (intToDigit)

data State      = A | I
type Rule       = [State]
type Cells      = [State]
type Neighbours = [State]

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

neighbours :: Int -> Cells -> Neighbours
neighbours i = pad 3 I . take 3 . drop (i-1) . edgecase
  where edgecase = if i==0 then (I:) else id

applyRule :: Rule -> Neighbours -> State
applyRule rule neighbours = (rule!!) $ case neighbours of 
  [A,A,A] -> 0
  [A,A,I] -> 1
  [A,I,A] -> 2
  [A,I,I] -> 3
  [I,A,A] -> 4
  [I,A,I] -> 5
  [I,I,A] -> 6
  [I,I,I] -> 7

toNeighbourList :: Cells -> [Neighbours]
toNeighbourList cells = map (flip neighbours cells) [1..length cells]

collapseNeighbours :: Rule -> [Neighbours] -> Cells
collapseNeighbours = map . applyRule

transition :: Rule -> Cells -> Cells
transition rule = collapseNeighbours rule . toNeighbourList

automata :: Int -> (Cells -> Cells)
automata = transition . intToRule

wolfram255 :: [Cells -> Cells]
wolfram255 = map automata [0..255]

instance Show State where
  show A = "■"
  show I = " "
  showList = showString . concat . map show

runner :: Int -> String -> IO ()
runner rule state = do
  putStrLn $ "\n\nRule " ++ show rule ++ ":\n"
  let computer = iterate $ automata rule
  let initial = map toState state
  mapM_ print $ take 10 $ computer initial

main :: IO ()
main = foldr (>>) epsilon $ map (flip runner starter) [0..255]
  where epsilon = return ()
        starter = "00010011011111000000000000010100101010"
