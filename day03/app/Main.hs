import AoCIO
import Log
import Text.Regex.PCRE

extractMatches :: String -> String -> [String]
extractMatches text pattern = getAllTextMatches (text =~ pattern :: AllTextMatches [] String)

evalMul :: String -> Int
evalMul s =
  let numberStrings = extractMatches s "[0-9]{1,3}"
      numbers = readIntList numberStrings
   in product numbers

main :: IO ()
main = do
  input <- readInput "input.txt"

  let pattern = "mul\\([0-9]{1,3},[0-9]{1,3}\\)"
  let matches = extractMatches input pattern
  verbosePrint matches

  let part1 = sum $ map evalMul matches

  print ("Part 1: " ++ show part1)

  let wrapped = "do()" ++ input ++ "don't()"
  verbosePrint "wrapped"
  verbosePrint wrapped
  let enabled = extractMatches ("do()" ++ input ++ "don't()") "(?s)do\\(\\).*?don't\\(\\)"
  verbosePrint "enabled"
  verbosePrint enabled
  let matches2 = extractMatches (concat enabled) pattern
  verbosePrint "matches"
  verbosePrint matches2
  let part2 = sum $ map evalMul matches2

  print ("Part 2: " ++ show part2)
