import Data.List (sort)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

getAmounts :: [String] -> [Int]
getAmounts strs = map sumStrings $ foldl accumulate [] strs
  where
    accumulate :: [[String]] -> String -> [[String]]
    accumulate [] x = [[x]]
    accumulate acc [] = [] : acc
    accumulate (head : acc) x = (head ++ [x]) : acc

sumStrings :: [String] -> Int
sumStrings strs = sum $ map read strs

getBiggest :: [String] -> Int
getBiggest strs = sum . take 3 . reverse . sort $ getAmounts strs

main :: IO ()
main = do
  file <- openFile "input.txt" ReadMode
  content <- hGetContents file
  let fileLines = lines content

  print $ getBiggest fileLines
