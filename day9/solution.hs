import Data.List (nub)
import Debug.Trace (trace)

-- (x, y)
type Coordinate = (Int, Int)

-- A move is represented by (dy, dy)
toMove :: String -> [Coordinate]
toMove line = replicate amount dir
  where
    dirLetter = head $ words line

    amount :: Int
    amount = read (words line !! 1)

    dir :: (Int, Int)
    dir
      | dirLetter == "R" = (1, 0)
      | dirLetter == "L" = (-1, 0)
      | dirLetter == "U" = (0, 1)
      | dirLetter == "D" = (0, -1)
      | otherwise = error "invalid input"

-- (Knot Pos, Visited) -> Move -> (Head Post, Visited)
doMove :: ([Coordinate], [[Coordinate]]) -> Coordinate -> ([Coordinate], [[Coordinate]])
doMove ((headX, headY) : xs, visited) (dx, dy) = (positions, positions : visited)
  where
    newHeadX = headX + dx
    newHeadY = headY + dy
    positions = (newHeadX, newHeadY) : reflect (newHeadX, newHeadY) (headX, headY) xs
doMove a b = error (show a ++ " " ++ show b)

-- New, Old, List
reflect :: Coordinate -> Coordinate -> [Coordinate] -> [Coordinate]
reflect new old [pt] = [getMove new pt]
reflect new old (pt : xs) = newpt : reflect newpt pt xs
  where
    newpt = getMove new pt
reflect x y xs = error "This will never happen"

getMove :: Coordinate -> Coordinate -> Coordinate
getMove (headX, headY) (tailX, tailY)
  | abs (headX - tailX) <= 1 && abs (headY - tailY) <= 1 = (tailX, tailY)
  | otherwise = (tailX + signum (headX - tailX), tailY + signum (headY - tailY))

main :: IO ()
main = do
  input <- readFile "input.txt"

  let (_, visited) = foldl doMove (replicate 10 (0, 0), [replicate 10 (0, 0)]) (concatMap toMove $ lines input)

  print $ (length . nub . map (!! 1)) visited
  print $ (length . nub . map (!! 9)) visited
