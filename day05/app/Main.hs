import AoCIO
import Data.List (elemIndex, partition, sortBy)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Log (verbosePrint)

reduceToMap :: [[Int]] -> Map.Map Int [Int]
reduceToMap = foldl insertPair Map.empty
  where
    insertPair acc [a, b] = Map.insertWith (++) a [b] acc
    insertPair acc _ = acc -- Ignore lists that don't have exactly two elements

validSequence :: Map.Map Int [Int] -> [Int] -> Bool
validSequence dep seq =
  all (\(a, b) -> index a < index b) pairs
  where
    index x = maybe (length seq) id (elemIndex x seq) -- Get the index of a page or a large value if missing
    pairs = [(a, b) | a <- Map.keys dep, b <- Map.findWithDefault [] a dep, a `elem` seq, b `elem` seq]

middlePage :: [Int] -> Int
middlePage xs = xs !! (length xs `div` 2)

partitionUpdates :: Map.Map Int [Int] -> [[Int]] -> ([[Int]], [[Int]])
partitionUpdates dependencies = partition (validSequence dependencies)

cmp :: [(Int, Int)] -> Int -> Int -> Ordering
cmp rules x y | (x, y) `elem` rules = LT
cmp rules x y | (y, x) `elem` rules = GT
cmp _ _ _ | otherwise           = EQ -- ???

convertToPairs :: [[Int]] -> [(Int, Int)]
convertToPairs = map toPair
  where
    toPair [x, y] = (x, y) -- Convert valid lists of length 2
    toPair invalid = error $ "Invalid input: " ++ show invalid

main :: IO ()
main = do
  input <- readInput "input.txt"
  let [top, bottom] = chunkByBlankLines input
  let rules = map (readIntList . splitOn "|") $ chunkByNewLines top
  let dependencies = reduceToMap rules
  let prints = map (readIntList . splitOn ",") $ parseLines bottom
  verbosePrint "Rules"
  verbosePrint rules
  verbosePrint "Prints"
  verbosePrint prints
  verbosePrint "Dependincies"
  verbosePrint dependencies

  let (validPrints, invalidPrints) = partitionUpdates dependencies prints
  let part1 = sum $ map middlePage validPrints
  print ("Part 1: " ++ show part1)

  let correctedPrints = map (sortBy $ cmp $ convertToPairs rules) invalidPrints
  let part2 = sum $ map middlePage correctedPrints
  print ("Part 2: " ++ show part2)
