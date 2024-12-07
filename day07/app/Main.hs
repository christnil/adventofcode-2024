import AoCIO
import Data.List.Split (splitOn)
import Log

customReadInt :: String -> Int
customReadInt s = case readInt s of
  Just n -> n
  Nothing -> error $ "Invalid integer: " ++ s

parseEquation :: String -> (Int, [Int])
parseEquation line = (target, numbers)
  where
    parts = splitOn ": " line
    target = customReadInt (head parts)
    numbers = map customReadInt (splitOn " " (last parts))

reverseNumbers :: (Int, [Int]) -> (Int, [Int])
reverseNumbers (t, n) = (t, reverse n)

concatInt :: Int -> Int -> Int
concatInt a b = read (show a ++ show b)

possibleEquation :: Int -> [Int] -> Bool
possibleEquation target [] = target == 0
possibleEquation target [x] = x == target
possibleEquation target numbers = addition || multiplication
  where
    x = head numbers
    rest = tail numbers
    addition = possibleEquation (target - x) rest
    multiplication = x /= 0 && target `mod` x == 0 && possibleEquation (target `div` x) rest

possibleEquation2 :: Int -> Int -> [Int] -> Bool
possibleEquation2 target current [] = target == current
possibleEquation2 target current numbers = addition || multiplication || concatenation
  where
    x = head numbers
    rest = tail numbers
    addition = possibleEquation2 target (current + x) rest
    multiplication = possibleEquation2 target (current * x) rest
    concatenation = current /= 0 && possibleEquation2 target (concatInt current x) rest

main :: IO ()
main = do
  input <- readInput "input.txt"
  let lines = parseLines input
  verbosePrint "lines"
  verbosePrint lines

  let equations = map parseEquation lines
  verbosePrint "equations"
  verbosePrint equations

  let validEquations = filter (uncurry possibleEquation) $ map reverseNumbers equations
  verbosePrint "validEquations"
  verbosePrint validEquations

  let validTargets = map fst validEquations

  let part1 = sum validTargets

  print ("Part 1: " ++ show part1)

  let validEquations2 = filter (\(t, n) -> possibleEquation2 t (head n) (tail n)) equations
  verbosePrint "validEquations2"
  verbosePrint validEquations2

  let validTargets2 = map fst validEquations2

  let part2 = sum validTargets2

  print ("Part 2: " ++ show part2)
