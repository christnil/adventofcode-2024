import AoCIO
import Data.List (sort)
import qualified Data.Map as Map
import Log

-- | Reduces a list of integers to a map of integers and their counts
frequencies :: [Int] -> Map.Map Int Int
frequencies xs = Map.fromListWith (+) [(x, 1) | x <- xs]

main :: IO ()
main = do
  input <- readInput "input.txt"
  let (l, r) = read2IntCols input

  verbosePrint "Left column:"
  verbosePrint l
  verbosePrint "Right column"
  verbosePrint r

  let distances = zipWith (\a b -> abs (b - a)) (sort l) (sort r)
  verbosePrint "Distances:"
  verbosePrint distances

  let s = sum distances

  print ("Part 1: " ++ show s)

  let freq = frequencies r

  verbosePrint "Frequencies:"
  verbosePrint freq

  let y = foldl (\acc x -> acc + (x * Map.findWithDefault 0 x freq)) 0 l

  print ("Part 2: " ++ show y)
