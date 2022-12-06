import Data.List (elemIndex, groupBy)
import Distribution.Simple.Utils (ordNub)

findUniqueSeries :: String -> Int -> Int
findUniqueSeries content seriesLength = seriesLength + length (notMatches content)
  where
    notMatches = takeWhile (not . isMatch . take seriesLength) . iterate (drop 1)

isMatch :: String -> Bool
isMatch arr = ordNub arr == arr

main :: IO ()
main = do
  content <- readFile "input.txt"

  print $ findUniqueSeries content 4
  print $ findUniqueSeries content 14
