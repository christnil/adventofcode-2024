import AoCIO
import Log

allDiffsInRange :: (Int, Int) -> [Int] -> Bool
allDiffsInRange (from, to) l = all (\x -> x >= from && x <= to) diffs
  where
    diffs = zipWith (-) (tail l) l

safeRow :: [Int] -> Bool
safeRow x = allDiffsInRange (-3, -1) x || allDiffsInRange (1, 3) x

safeRowWithRemoval :: [Int] -> Bool
safeRowWithRemoval xs = any safeRow (removeOne xs)

removeOne :: [a] -> [[a]]
removeOne [] = []
removeOne (x : xs) = xs : map (x :) (removeOne xs)

safeWithDamperRow :: [Int] -> Bool
safeWithDamperRow x = safeRow x || safeRowWithRemoval x

main :: IO ()
main = do
  input <- readInput "input.txt"
  let rows = readIntRows input

  verbosePrint "Rows:"
  verbosePrint rows

  let validRows = filter safeRow rows

  verbosePrint "Valid rows:"
  verbosePrint validRows

  let part1 = length validRows

  print ("Part 1: " ++ show part1)

  let validRowsWithDamper = filter safeWithDamperRow rows

  verbosePrint "Valid rows:"
  verbosePrint validRowsWithDamper

  let part2 = length validRowsWithDamper

  print ("Part 2: " ++ show part2)
