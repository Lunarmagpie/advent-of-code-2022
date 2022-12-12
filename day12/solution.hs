import Data.Char (ord)
import Data.Map (Map, empty, insert, member)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, isJust, isNothing)
import Debug.Trace (trace)
import Util (mapSnd)

toElevation :: [[Char]] -> [[Int]]
toElevation = (map . map) (isElevation . subtract 97 . ord)
  where
    isElevation (-14) = 0
    isElevation (-28) = 25
    isElevation a = a

findSolution :: [[Int]] -> Map (Int, Int) Int -> Int -> (Int, Int) -> (Int, Int) -> (Int, Map (Int, Int) Int)
findSolution elevations travelled iteration end (x, y)
  | end == (x, y) = (iteration, nwTravelled)
  | (x, y) `member` travelled && fromJust (M.lookup (x, y) travelled) <= iteration = (maxBound, travelled)
  | x < 0 || y < 0 || x >= length (head elevations) || y >= length elevations = (maxBound, travelled)
  | otherwise = foldl accumulate (maxBound, nwTravelled) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
  where
    nwTravelled = insert (x, y) iteration travelled

    accumulate :: (Int, Map (Int, Int) Int) -> (Int, Int) -> (Int, Map (Int, Int) Int)
    accumulate (value, travelled) (x2, y2)
      | x2 < 0 || y2 < 0 || x2 >= length (head elevations) || y2 >= length elevations = (value, travelled)
      | diff <= 1 = (min value score, nextTravelled)
      | otherwise = (value, travelled)
      where
        diff = ((elevations !! y2) !! x2) - ((elevations !! y) !! x)
        (score, nextTravelled) = findSolution elevations travelled (iteration + 1) end (x2, y2)

find :: Char -> String -> Maybe Int
find item arr = if item `elem` arr then Just (length $ takeWhile (/= item) arr) else Nothing

find2d :: Char -> [String] -> (Int, Int)
find2d item arr = do
  let index = length $ takeWhile isNothing $ map (find item) arr
  (fromJust $ find item (arr !! index), index)

enumerate2d :: [[b]] -> [(Int, [(Int, b)])]
enumerate2d = zip [0 ..] . map (zip [0 ..])

getAPoints :: [[Char]] -> [(Int, Int)]
getAPoints = concatMap list2tuple . mapSnd (filter (\a -> snd a == 'a')) . enumerate2d
  where
    list2tuple (num, items) = map (\(a, _) -> (a, num)) items



main :: IO ()
main = do
  content <- readFile "input.txt"

  let elevation = toElevation (lines content)

  let points = getAPoints (lines content)

  let endIndex = find2d 'E' (lines content)


  let startIndex = find2d 'S' (lines content)
  let solve = fst . findSolution elevation empty 0 endIndex

  putStrLn ("Part One: " ++ show (solve startIndex))
  putStrLn ("Part Two: " ++ show (minimum $ map solve points))
