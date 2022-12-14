import Data.List (sortOn)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (isJust)
import GHC.Float (float2Int)

type Line = ((Int, Int), (Int, Int))

type Grid = M.Map (Int, Int) Char

toLines :: String -> [Line]
toLines = map (toLine . map (map read . splitOn ",") . take 2) . init . takeWhile (not . null) . iterate (drop 1) . splitOn " -> "
  where
    toLine [[x1, y1], [x2, y2]] = ((x1, y1), (x2, y2))

toPoints :: String -> [(Int, Int)]
toPoints = map (toPoint . splitOn ",") . splitOn " -> "
  where
    toPoint [x1, y1] = (read x1, read y1)

repeatList :: [a] -> [a]
repeatList = concat . repeat

getMap :: [[Line]] -> Grid
getMap = foldl (foldl insertLine) M.empty
  where
    insertLine :: Grid -> Line -> Grid
    insertLine grid ((x1, y1), (x2, y2))
      | y1 < y2 && x1 == x2 = insert [(x1, i) | i <- [y1 .. y2]]
      | y1 >= y2 && x1 == x2 = insert [(x1, i) | i <- [y2 .. y1]]
      | x1 < x2 && y1 == y2 = insert [(i, y1) | i <- [x1 .. x2]]
      | x1 >= x2 && y1 == y2 = insert [(i, y1) | i <- [x2 .. x1]]
      | otherwise = error "?????"
      where
        insert = foldr (`M.insert` '#') grid

dropSand1 :: Int -> Maybe Grid -> (Int, Int) -> Maybe Grid
dropSand1 _ Nothing _ = Nothing
dropSand1 max (Just grid) (x, y)
  | y > max = Nothing
  | not ((x, y + 1) `M.member` grid) = dropSand1 max (Just grid) (x, y + 1)
  | not ((x - 1, y + 1) `M.member` grid) = dropSand1 max (Just grid) (x -1, y + 1)
  | not ((x + 1, y + 1) `M.member` grid) = dropSand1 max (Just grid) (x + 1, y + 1)
  | otherwise = Just $ M.insert (x, y) '0' grid

dropSand2 :: Int -> Maybe Grid -> (Int, Int) -> Maybe Grid
dropSand2 _ Nothing _ = Nothing
dropSand2 max (Just grid) (x, y)
  | y >= max - 1 = Just $ M.insert (x, y) '0' grid
  | not ((x, y + 1) `M.member` grid) = dropSand2 max (Just grid) (x, y + 1)
  | not ((x - 1, y + 1) `M.member` grid) = dropSand2 max (Just grid) (x -1, y + 1)
  | not ((x + 1, y + 1) `M.member` grid) = dropSand2 max (Just grid) (x + 1, y + 1)
  | not ((x, y) `M.member` grid) = Just $ M.insert (x, y) '0' grid
  | otherwise = Nothing

main = do
  content <- readFile "input.txt"

  let points = (getMap . map toLines . lines) content
  let pointsList = (concatMap toPoints . lines) content
  let max = maximum (map snd pointsList)

  print $ ((+) (-1) . length . takeWhile isJust) $ scanl (dropSand1 max) (Just points) (repeat (500, 0))
  print $ ((+) (-1) . length . takeWhile isJust) $ scanl (dropSand2 (max + 2)) (Just points) (repeat (500, 0))
