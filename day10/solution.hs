import Data.List (last)
import Debug.Trace (trace)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

toCycles :: [String] -> [Int]
toCycles = reverse . tail . foldl addCycle [1]

addCycle :: [Int] -> String -> [Int]
addCycle numbers line
  | line == "noop" = listHead : numbers
  | otherwise = (amount + listHead) : replicate 2 listHead ++ newLst
  where
    newLst = tail numbers
    listHead = head numbers
    amount = read $ words line !! 1

-- (Index, Register Value)
drawLine :: [(Int, Int)] -> String
drawLine = map (\(x, reg) -> if abs (x - reg) <= 1 then '#' else '.')

partOne :: String -> Int
partOne input = sum $ map (\a -> a * (cycles !! (a - 1))) [20, 60, 100, 140, 180, 220]
  where
    cycles = toCycles $ lines input

partTwo :: String -> String
partTwo = unlines . map (drawLine . enumerate) . chunksOf 40 . toCycles . lines

main :: IO ()
main = do
  content <- readFile "input.txt"

  print $ partOne content
  putStrLn $ partTwo content
