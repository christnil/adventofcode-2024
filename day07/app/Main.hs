import AoCIO
import Data.List.Split (splitOn)
import Log

customReadInt :: String -> Int
customReadInt s = case readInt s of
  Just n -> n
  Nothing -> error $ "Invalid integer: " ++ s

parseEquation :: String -> (Int, [Int])
parseEquation line = (target, reverse numbers)
  where
    parts = splitOn ": " line
    target = customReadInt (head parts)
    numbers = map customReadInt (splitOn " " (last parts))

possibleEquation :: Int -> [Int] -> Bool
possibleEquation target [] = target == 0
possibleEquation target [x] = x == target
possibleEquation target numbers = addition || multiplication
  where
    x = head numbers
    rest = tail numbers
    addition = possibleEquation (target - x) rest
    multiplication = x /= 0 && target `mod` x == 0 && possibleEquation (target `div` x) rest

main :: IO ()
main = do
  input <- readInput "input.txt"
  let lines = parseLines input
  verbosePrint "lines"
  verbosePrint lines

  let equations = map parseEquation lines
  verbosePrint "equations"
  verbosePrint equations

  let validEquations = filter (uncurry possibleEquation) equations
  verbosePrint "validEquations"
  verbosePrint validEquations

  let validTargets = map fst validEquations

  let part1 = sum validTargets

  print ("Part 1: " ++ show part1)

  let part2 = 1

  print ("Part 2: " ++ show part2)
