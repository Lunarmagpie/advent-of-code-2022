import Data.Char (digitToInt)

map2d = map . map

directions = [(1, 0), (-1, 0), (0, 1), (0, -1)]

to2DArray :: String -> [[Int]]
to2DArray = map2d digitToInt . lines

isVisible :: [[Int]] -> (Int, Int) -> Bool
isVisible array (x, y) = any (inner (x, y)) directions
  where
    number = array !! x !! y
    inner :: (Int, Int) -> (Int, Int) -> Bool
    inner (x, y) (dx, dy)
      | x + dx < 0 || x + dx >= length array = True
      | y + dy < 0 || y + dy >= length (head array) = True
      | otherwise = array !! (x + dx) !! (y + dy) < number && inner (x + dx, y + dy) (dx, dy)

scenicScore :: [[Int]] -> (Int, Int) -> Int
scenicScore array (x, y) = product $ map (inner (x, y)) directions
  where
    number = array !! x !! y
    inner :: (Int, Int) -> (Int, Int) -> Int
    inner (x, y) (dx, dy)
      | x + dx < 0 || x + dx >= length array = 0
      | y + dy < 0 || y + dy >= length (head array) = 0
      | array !! (x + dx) !! (y + dy) >= number = 1
      | otherwise = 1 + inner (x + dx, y + dy) (dx, dy)

coordinateMap :: ([[Int]] -> (Int, Int) -> b) -> [[Int]] -> [[b]]
coordinateMap f arr = map2d (f arr) $ map (row [0 .. width -1]) [0 .. height -1]
  where
    row :: [Int] -> Int -> [(Int, Int)]
    row [x] n = [(n, x)]
    row (x : xs) n = (n, x) : row xs n
    row [] _ = []

    width = length arr
    height = length (head arr)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let arr = to2DArray input

  let partOne = sum $ map sum $ map2d fromEnum (coordinateMap isVisible arr)
  let partTwo = maximum $ map maximum (coordinateMap scenicScore arr)

  putStrLn $ "Part one result:" ++ show partOne
  putStrLn $ "Part two result:" ++ show partTwo
