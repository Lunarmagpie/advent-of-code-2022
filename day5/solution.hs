import Data.List (isPrefixOf, transpose)
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)
import Text.Read (readMaybe)
import Debug.Trace (trace)

dropEnd n = reverse . drop n . reverse

takeEnd n = reverse . take n . reverse

replace list n new = left ++ [new] ++ drop 1 right
  where
    (left, right) = splitAt n list

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

parseStacks :: [String] -> [String]
parseStacks = map trim . filter isNumLine . transpose . tail . reverse
  where
    isNumLine a = not ("[" `isPrefixOf` a || " " `isPrefixOf` a || "]" `isPrefixOf` a)
    trim = takeWhile (/= ' ')

-- Count, From, To
parseInput :: String -> (Int, Int, Int)
parseInput line = (head words_, words_ !! 1 - 1, words_ !! 2 - 1)
  where
    words_ = mapMaybe readMaybe $ words line

applyMove :: ([Char] -> [Char])  -> [[Char]] -> (Int, Int, Int) -> [[Char]]
applyMove app chars (count, from, to) = new2
  where
    dropped = app $ takeEnd count (chars !! from)
    new = replace chars from (dropEnd count (chars !! from))
    new2 = replace new to ((new !! to) ++ dropped)

crateMover9000 :: [[Char]] -> [(Int, Int, Int)] -> [[Char]]
crateMover9000 = foldl (applyMove reverse)

crateMover9001 :: [[Char]] -> [(Int, Int, Int)] -> [[Char]]
crateMover9001 = foldl (applyMove id)

formatOutput :: [String] -> String
formatOutput = map last

main :: IO ()
main = do
  content <- readFile "input.txt"

  let stacks = (parseStacks . takeWhile (not . null) . lines) content
  let moves = (map parseInput . tail . dropWhile (not . null) . lines) content

  print $ formatOutput $ crateMover9000 stacks moves
  print $ formatOutput $ crateMover9001 stacks moves
