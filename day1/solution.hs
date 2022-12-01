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
getBiggest strs = maximum $ getAmounts strs

getThreeBiggest :: [String] -> Int
getThreeBiggest = sum . take 3 . reverse . sort . getAmounts

main :: IO ()
main = do
  file <- openFile "input.txt" ReadMode
  content <- hGetContents file
  let fileLines = lines content

  putStr $ "The biggest is: " ++ show (getBiggest fileLines)
  putStr $ "\nThe three biggest are: " ++ show (getThreeBiggest fileLines)
