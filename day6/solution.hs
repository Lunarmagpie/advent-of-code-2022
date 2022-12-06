import Data.List (elemIndex, groupBy, nub)

findUniqueSeries :: Eq a => [a] -> Int -> Int
findUniqueSeries content seriesLength = seriesLength + notMatches content
  where
    notMatches = length . takeWhile (not . isMatch . take seriesLength) . iterate (drop 1)

isMatch :: Eq a => [a] -> Bool
isMatch arr = nub arr == arr

main :: IO ()
main = do
  content <- readFile "input.txt"

  print $ findUniqueSeries content 4
  print $ findUniqueSeries content 14
