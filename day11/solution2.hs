{-# LANGUAGE NamedFieldPuns #-}

import Data.List (isSubsequenceOf, sort)
import Debug.Trace (trace)

data Operation = AddN Int | MultN Int | MultSelf deriving (Show)

data Monkey = Monkey
  { items :: [Int],
    operation :: Operation,
    test :: Int,
    throwTrue :: Int,
    throwFalse :: Int,
    inspected :: Int
  }
  deriving (Show)

splitOn :: (Char -> Bool) -> String -> [String]
splitOn f [] = []
splitOn f [x] = []
splitOn f s =
  a :
  splitOn
    f
    ( case b of
        [] -> []
        s -> tail s
    )
  where
    (a, b) = break f s

after f = tail . dropWhile (not . f)

repeatFunc n f = last . take (n + 1) . iterate f

replace list n new = left ++ [new] ++ drop 1 right
  where
    (left, right) = splitAt n list

genMonkeys :: [String] -> [Monkey]
genMonkeys = map (intoMoney . take 6) . takeWhile (not . null) . iterate (drop 7)
  where
    intoMoney :: [String] -> Monkey
    intoMoney monkeyData =
      Monkey
        { items = (map read . splitOn (== ',') . after (== ':')) $ monkeyData !! 1,
          operation = if '+' `elem` monkeyData !! 2 then parseAdd $ monkeyData !! 2 else parseMult $ monkeyData !! 2,
          test = read $ drop 21 $ monkeyData !! 3,
          throwTrue = read $ drop 29 $ monkeyData !! 4,
          throwFalse = read $ drop 30 $ monkeyData !! 5,
          inspected = 0
        }
    parseAdd line = AddN (read $ after (== '+') line)
    parseMult line = if "old" `isSubsequenceOf` after (== '*') line then MultSelf else MultN (read $ after (== '*') line)

doOneMonkey :: Int -> [Monkey] -> [Monkey]
doOneMonkey number monkeys = applyThrows (replace monkeys number newSlf) throws
  where
    Monkey
      { items,
        operation,
        test = _test,
        throwTrue,
        throwFalse,
        inspected
      } = monkeys !! number
    throws = map getThrow items

    denominator = product $ map test monkeys

    -- Return (Worry Amount, Monkey)
    getThrow :: Int -> (Int, Int)
    getThrow worry = throwTo value
      where
        value = operate worry operation

    operate old (AddN v) = old + v
    operate old (MultN v) = old * v
    operate old MultSelf = old * old

    throwTo :: Int -> (Int, Int)
    throwTo i = if i `mod` _test == 0 then (i `mod` denominator, throwTrue) else (i `mod` denominator, throwFalse)

    newSlf =
      Monkey
        { items = [],
          operation,
          test = _test,
          throwTrue,
          throwFalse,
          inspected = inspected + length items
        }

applyThrows :: [Monkey] -> [(Int, Int)] -> [Monkey]
applyThrows = foldl applyThrow

applyThrow :: [Monkey] -> (Int, Int) -> [Monkey]
applyThrow monkeys (amount, to) = replace monkeys to newMonkey
  where
    Monkey
      { items,
        operation,
        test,
        throwTrue,
        throwFalse,
        inspected
      } = monkeys !! to

    newMonkey =
      Monkey
        { items = sort (amount : items),
          operation,
          test,
          throwTrue,
          throwFalse,
          inspected
        }

doRound :: [Monkey] -> [Monkey]
doRound monkeys = foldl (flip doOneMonkey) monkeys [0 .. (length monkeys - 1)]

main :: IO ()
main = do
  content <- readFile "input.txt"

  let monkeys = genMonkeys (lines content)

  let endMonkeys = repeatFunc 10000 doRound monkeys

  print $ map inspected endMonkeys
  print $ (product . take 2 . reverse . sort . map inspected) endMonkeys
