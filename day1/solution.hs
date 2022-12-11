import Data.List (sort)
import Data.List.Split (splitOn)

partOne :: String -> Int
partOne = maximum . map (sum . map read . lines) . splitOn "\n\n"

partTwo :: String -> Int
partTwo = sum . take 3 . reverse . sort . map (sum . map read . lines) . splitOn "\n\n"

main :: IO ()
main = do
  content <- readFile "input.txt"

  putStr $ "The biggest is: " ++ show (partOne content)
  putStr $ "\nThe three biggest are: " ++ show (partTwo content)
