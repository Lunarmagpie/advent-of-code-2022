import Data.Char (ord)
import Data.List
import System.IO (IOMode (ReadMode), hGetContents, openFile)

inBoth :: (String, String) -> [Char]
inBoth (left, right) = filter (`elem` right) left

inThree :: (String, String, String) -> Char
inThree (a, b, c) = head $ inBoth (aAndB, c)
  where
    aAndB = inBoth (a, b)

charScore :: Char -> Int
charScore char
  | ord char >= 97 = ord char - 96
  | otherwise = ord char - 65 + 27

groupByThree :: [String] -> [(String, String, String)]
groupByThree (x : y : z : xs) = (x, y, z) : groupByThree xs
groupByThree [] = []
groupByThree _ = error "INVALID INPUT"

getTotal :: [String] -> Int
getTotal lines = sum $ map (charScore . inThree) $ groupByThree lines

main :: IO ()
main = do
  file <- openFile "input.txt" ReadMode
  content <- hGetContents file
  let fileLines = lines content

  putStr $ "The total is: " ++ show (getTotal fileLines)
