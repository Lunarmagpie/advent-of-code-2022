import Data.List (elemIndex, groupBy, nub, tails)

findUniqueSeries :: Eq a => [a] -> Int -> Int
findUniqueSeries content seriesLength = seriesLength + notMatchesCount content
  where
    notMatchesCount = length . takeWhile (not . isMatch . take seriesLength) . tails

isMatch :: Eq a => [a] -> Bool
isMatch arr = nub arr == arr

main :: IO ()
main = do
  content <- readFile "input.txt"

  print $ findUniqueSeries content 4
  print $ findUniqueSeries content 14
