import Data.List (sort)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

-- Rock     1
-- Paper    2
-- Scisors  3

data Shape = Rock | Paper | Scisors deriving (Show)

instance Enum Shape where
  fromEnum enum = case enum of
    Rock -> 1
    Paper -> 2
    Scisors -> 3

  toEnum = undefined

data WinLoseTie = Win | Lose | Tie deriving (Show)

instance Enum WinLoseTie where
  fromEnum enum = case enum of
    Win -> 6
    Lose -> 0
    Tie -> 3

  toEnum = undefined

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
-- Rock first
getScore (Rock, Win) = fromEnum Win + fromEnum Paper
getScore (Rock, Lose) = fromEnum Scisors
-- Paper first
getScore (Paper, Win) = fromEnum Win + fromEnum Scisors
getScore (Paper, Lose) = fromEnum Rock
-- Scisors first
getScore (Scisors, Win) = fromEnum Win + fromEnum Rock
getScore (Scisors, Lose) = fromEnum Paper
getScore (a, b) = 3 + fromEnum a

main :: IO ()
main = do
  file <- openFile "input.txt" ReadMode
  content <- hGetContents file
  let fileLines = lines content

  putStr $ "The score is: " ++ show (sum . map getScore $ getShapes fileLines)
