import Data.Char
import Data.List
import System.IO

getAmounts :: [String] -> [Int]
getAmounts strs = map sumStrings $ foldl accumulate [] strs
  where
    accumulate :: [[String]] -> String -> [[String]]
    accumulate acc [] = [] : acc
    accumulate (head : acc) x = (head ++ [x]) : acc
    accumulate [] x = [[x]]

sumStrings :: [String] -> Int
sumStrings strs = sum $ map read strs

getBiggest :: [String] -> Int
getBiggest strs = maximum $ getAmounts strs

main :: IO ()
main = do
  file <- openFile "input.txt" ReadMode
  content <- hGetContents file
  let fileLines = lines content

  print $ getBiggest fileLines
