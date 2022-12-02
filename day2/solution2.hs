import Data.List (sort)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

-- Rock     1
-- Paper    2
-- Scisors  3

data Shape = None | Rock | Paper | Scisors deriving (Enum, Eq, Show)

data WinLoseTie = Win | Lose | Tie deriving (Eq, Show)

fromEnum' enum = case enum of
  Win -> 6
  Lose -> 0
  Tie -> 3

getShapes :: [String] -> [(Shape, WinLoseTie)]
getShapes = map getShapes
  where
    getShapes :: String -> (Shape, WinLoseTie)
    getShapes line = (getShape (head line), getWinLoseTie (last line))

    getShape :: Char -> Shape
    getShape 'A' = Rock
    getShape 'B' = Paper
    getShape 'C' = Scisors
    getShape _ = error "Not a valid Shape"

    getWinLoseTie :: Char -> WinLoseTie
    getWinLoseTie 'X' = Lose
    getWinLoseTie 'Y' = Tie
    getWinLoseTie 'Z' = Win
    getWinLoseTie _ = error "Not a valid WinLoseTie"

getScore :: (Shape, WinLoseTie) -> Int
getScore (a, b)
  | b == Tie = fromEnum' Tie + fromEnum a
  -- Rock first
  | a == Rock && b == Win = fromEnum' Win + fromEnum Paper
  | a == Rock && b == Lose = fromEnum Scisors
  -- Paper first
  | a == Paper && b == Win = fromEnum' Win + fromEnum Scisors
  | a == Paper && b == Lose = fromEnum Rock
  -- Scisors First
  | a == Scisors && b == Win = fromEnum' Win + fromEnum Rock
  | a == Scisors && b == Lose = fromEnum Paper
  | otherwise = error ("No case " ++ show (a, b))

main :: IO ()
main = do
  file <- openFile "input.txt" ReadMode
  content <- hGetContents file
  let fileLines = lines content

  putStr $ "The score is: " ++ show (sum . map getScore $ getShapes fileLines)
