module AoCIO
  ( readInput,
    parseLines,
    parseWords,
    parseCSV,
    readInt,
    readIntList,
    chunkByBlankLines,
    chunkByNewLines,
    readIntRows,
    read2IntRows,
    read3IntRows,
    readIntCols,
    read2IntCols,
    read3IntCols,
  )
where

import Data.List (transpose)
import Data.List.Split (splitOn)

-- | Reads the entire input from a given file.
readInput :: FilePath -> IO String
readInput = readFile

-- | Parses the input into a list of lines.
parseLines :: String -> [String]
parseLines = lines

-- | Parses a string into a list of words, splitting on spaces.
parseWords :: String -> [String]
parseWords = words

-- | Parses CSV (comma-separated values) into a list of lists.
parseCSV :: String -> [[String]]
parseCSV = map (splitOn ",") . lines

-- | Converts a string to an integer. Returns `Nothing` if parsing fails.
readInt :: String -> Maybe Int
readInt s = case reads s of
  [(n, "")] -> Just n
  _ -> Nothing

-- | Converts a list of strings into a list of integers. Skips invalid entries.
readIntList :: [String] -> [Int]
readIntList = map readIntSafe
  where
    readIntSafe s = case readInt s of
      Just n -> n
      Nothing -> error $ "Invalid integer: " ++ s

-- | Splits input into chunks separated by blank lines, ignoring leading and trailing empty chunks.
chunkByBlankLines :: String -> [String]
chunkByBlankLines = filter (not . null) . splitOn "\n\n"

-- | Splits input into chunks separated by blank lines, ignoring leading and trailing empty chunks.
chunkByNewLines :: String -> [String]
chunkByNewLines = filter (not . null) . splitOn "\n"

readIntRows :: String -> [[Int]]
readIntRows input = map (readIntList . parseWords) $ parseLines input

readIntCols :: String -> [[Int]]
readIntCols = transpose . readIntRows

read2IntCols :: String -> ([Int], [Int])
read2IntCols input =
  let cols = readIntCols input
   in case cols of
        (c1 : c2 : _) -> (c1, c2)
        _ -> error "Does not contain 2 columns"

read3IntCols :: String -> ([Int], [Int], [Int])
read3IntCols input =
  let cols = readIntCols input
   in case cols of
        (c1 : c2 : c3 : _) -> (c1, c2, c3)
        _ -> error "Does not contain 3 columns"

read2IntRows :: String -> ([Int], [Int])
read2IntRows input =
  let rows = readIntRows input
   in case rows of
        (r1 : r2 : _) -> (r1, r2)
        _ -> error "Does not contain 2 rows"

read3IntRows :: String -> ([Int], [Int], [Int])
read3IntRows input =
  let rows = readIntRows input
   in case rows of
        (r1 : r2 : r3 : _) -> (r1, r2, r3)
        _ -> error "Does not contain 3 rows"
