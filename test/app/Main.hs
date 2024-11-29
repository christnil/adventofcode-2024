import AoCIO
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)

parseLine :: String -> Maybe Int
parseLine str =
  case digits of
    [] -> Nothing
    [x] -> Just (read [x, x])
    _ -> Just (read [head digits, last digits])
  where
    digits = filter isDigit str

main :: IO ()
main = do
  input <- readInput "input.txt"

  let chunks = chunkByNewLines input
  putStrLn "Chunks:"
  mapM_ print chunks

  let numbers = map parseLine chunks
  print numbers

  let total = sum . map (fromMaybe 0) $ numbers
  print total
