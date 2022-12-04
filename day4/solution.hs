import Control.Arrow ((***))
import Control.Monad (join)
import Data.Array (Ix (range))
import Data.List (isSubsequenceOf)
import Debug.Trace (trace)

bool2Int :: Bool -> Int
bool2Int False = 0
bool2Int True = 1

splitWhere :: (a -> Bool) -> [a] -> ([a], [a])
splitWhere split l = (left, drop 1 right)
  where
    (left, right) = break split l

containsAny :: ([Int], [Int]) -> Bool
containsAny (a, b) = any (`elem` a) b

toRange :: String -> [Int]
toRange line = range $ join (***) read lf
  where
    lf = splitWhere ('-' ==) line

containsOther :: String -> (([Int], [Int]) -> Bool) -> Bool
containsOther line contains = contains (left, right) || contains (right, left)
  where
    (left, right) = join (***) toRange $ splitWhere (',' ==) line

main :: IO ()
main = do
  content <- readFile "input.txt"
  let fileLines = lines content

  putStr $ "Part one: " ++ show (sum $ map (bool2Int . flip containsOther (uncurry isSubsequenceOf)) fileLines)
  putStr $ "\nPart two: " ++ show (sum $ map (bool2Int . flip containsOther containsAny) fileLines)
