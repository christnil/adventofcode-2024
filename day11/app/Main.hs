import AoCIO
import Data.List.Split (splitOn)
import Log (verbosePrint)

-- split number into two parts with half of the digits in each part
splitDigit :: String -> [Int]
splitDigit n = let (a, b) = splitAt (length n `div` 2) n in [read a, read b]

iteration :: Int -> [Int]
iteration n | n == 0 = [1]
            | even (length (show n)) = splitDigit $ show n
            | otherwise = [n * 2024]

groupAndCount :: [Int] -> [(Int, Int)]
groupAndCount [] = []
groupAndCount (x:xs) = (x, length $ takeWhile (== x) (x:xs)) : groupAndCount (dropWhile (== x) xs)

nextArray :: [Int] -> [Int]
nextArray x = x >>= iteration

main :: IO ()
main = do
  input <- readInput "input.txt"
  let rocks = readIntList $ splitOn " " input
  verbosePrint "Rocks"
  verbosePrint rocks

  let iteration1 = rocks >>= iteration
  verbosePrint "Iteration 1"
  verbosePrint iteration1

  let iteration25 = iterate nextArray rocks !! 25

  let part1 = length iteration25
  print ("Part 1: " ++ show part1)


  let iteration75 = iterate nextArray rocks !! 75
  let part2 = length iteration75
  print ("Part 2: " ++ show part2)
