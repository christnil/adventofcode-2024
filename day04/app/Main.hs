import AoCIO
import Data.List (isPrefixOf, transpose)
import Log
import Text.Regex.PCRE

diagonals :: [[a]] -> [[a]]
diagonals matrix = leftDiagonals ++ rightDiagonals
  where
    rows = length matrix
    cols = length (head matrix)
    leftDiagonals =
      [[matrix !! (i + d) !! i | i <- [0 .. min (rows - d) cols - 1]] | d <- [0 .. rows - 1]]
        ++ [[matrix !! i !! (i + d) | i <- [0 .. min rows (cols - d) - 1]] | d <- [1 .. cols - 1]]
    rightDiagonals =
      [[matrix !! (i + d) !! (cols - 1 - i) | i <- [0 .. min (rows - d) cols - 1]] | d <- [0 .. rows - 1]]
        ++ [[matrix !! i !! (cols - 1 - (i + d)) | i <- [0 .. min rows (cols - d) - 1]] | d <- [1 .. cols - 1]]

-- Funktion för att kombinera alla rader, kolumner och diagonaler
allLines :: [[a]] -> [[a]]
allLines matrix = rows ++ cols ++ diags
  where
    rows = matrix
    cols = transpose matrix
    diags = diagonals matrix

countOccurrences :: String -> String -> Int
countOccurrences sub str
  | null sub || null str = 0
  | sub `isPrefixOf` str = 1 + countOccurrences sub (drop (length sub) str)
  | otherwise = countOccurrences sub (tail str)

findXmas :: [String] -> [Int]
findXmas matr = [newValue r c | r <- [1 .. (numRows - 2)], c <- [1 .. (numCols - 2)]]
  where
    numRows = length matr
    numCols = length (head matr)

    -- Villkorsfunktion för att bestämma det nya värdet
    newValue :: Int -> Int -> Int
    newValue row col
      | mid /= 'A' = 0 -- Sätt '#' på huvuddiagonalen
      | diag1Ok && diag2Ok = 1 -- Sätt '*' på alla jämna positioner
      | otherwise = 0
      where
        mid = (matr !! row) !! col
        tl = (matr !! (row - 1)) !! (col - 1)
        tr = (matr !! (row - 1)) !! (col + 1)
        bl = (matr !! (row + 1)) !! (col - 1)
        br = (matr !! (row + 1)) !! (col + 1)
        diag1Ok = (tl == 'M' && br == 'S') || (tl == 'S' && br == 'M')
        diag2Ok = (tr == 'M' && bl == 'S') || (tr == 'S' && bl == 'M')

main :: IO ()
main = do
  input <- readInput "input.txt"
  let strings = parseLines input

  verbosePrint strings

  let allStrings = allLines strings
  verbosePrint allStrings

  let xmasCounts = map (countOccurrences "XMAS") allStrings
  let samxCounts = map (countOccurrences "SAMX") allStrings
  verbosePrint xmasCounts
  verbosePrint samxCounts

  let part1 = sum (xmasCounts ++ samxCounts)

  print ("Part 1: " ++ show part1)

  let part2 = sum $ findXmas strings

  print ("Part 2: " ++ show part2)
