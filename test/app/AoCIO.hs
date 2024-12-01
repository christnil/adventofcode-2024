module AoCIO
  ( readInput,
    parseLines,
    parseWords,
    parseCSV,
    readInt,
    readIntList,
    chunkByBlankLines,
    chunkByNewLines,
  )
where

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
