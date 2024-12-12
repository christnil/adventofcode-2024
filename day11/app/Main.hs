import AoCIO
import Data.List (groupBy, sortBy)
import Data.List.Split (splitOn)
import Log (verbosePrint)
import Data.Function (on)

-- split number into two parts with half of the digits in each part
splitDigit :: String -> [Int]
splitDigit n = let (a, b) = splitAt (length n `div` 2) n in [read a, read b]

iteration :: (Int, Int) -> [(Int, Int)]
iteration (n, c)  | n == 0 = [(1, c)]
                  | even (length (show n)) = map (\x -> (x, c)) $ splitDigit $ show n
                  | otherwise = [(n * 2024, c)]

mergeCounts :: [(Int, Int)] -> [(Int, Int)]
mergeCounts pairs =
  map mergeGroup grouped
  where
    sorted = sortBy (compare `on` fst) pairs
    grouped = groupBy ((==) `on` fst) sorted
    mergeGroup grp = (fst (head grp), sum (map snd grp))

nextArray :: [(Int, Int)] -> [(Int, Int)]
nextArray x = mergeCounts(x >>= iteration)

main :: IO ()
main = do
  input <- readInput "input.txt"
  let rocks = readIntList $ splitOn " " input
  let rockCounts = map (\x -> (x, 1)) rocks
  verbosePrint "Rocks"
  verbosePrint rockCounts

  let iteration1 = rockCounts >>= iteration
  verbosePrint "Iteration 1"
  verbosePrint iteration1

  let iteration25 = iterate nextArray rockCounts !! 25

  let part1 = sum $ map snd iteration25
  print ("Part 1: " ++ show part1)


  let iteration75 = iterate nextArray rockCounts !! 75
  let part2 = sum $ map snd iteration75
  print ("Part 2: " ++ show part2)
