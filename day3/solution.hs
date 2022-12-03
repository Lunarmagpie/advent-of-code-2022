import Data.Char (ord)
import Data.List
import System.IO (IOMode (ReadMode), hGetContents, openFile)

inBoth :: (String, String) -> Char
inBoth (left, right) = head $ filter (`elem` right) left

charScore :: Char -> Int
charScore char
  | ord char >= 97 = ord char - 96
  | otherwise = ord char - 64 + 26

getScores :: [Char] -> Int
getScores chars = sum $ map charScore chars

-- Return the priority for a single rucksack.
getPriority :: String -> Int
getPriority line = charScore $ inBoth (left_pocket, right_pocket)
  where
    left_pocket = take (length line `div` 2) line
    right_pocket = drop (length line `div` 2) line

getTotal :: [String] -> Int
getTotal lines = sum $ map getPriority lines

main :: IO ()
main = do
  file <- openFile "input.txt" ReadMode
  content <- hGetContents file
  let fileLines = lines content

  putStr $ "The total is: " ++ show (getTotal fileLines)
