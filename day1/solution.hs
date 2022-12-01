import Data.List (sort)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

getAmounts :: [String] -> [Int]
getAmounts = foldl accumulate []
  where
    accumulate :: [Int] -> String -> [Int]
    accumulate [] x = [read x]
    accumulate acc [] = 0 : acc
    accumulate (head : acc) x = (read x + head) : acc

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
