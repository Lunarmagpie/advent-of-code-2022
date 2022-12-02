import Data.List (sort)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

-- Rock     1
-- Paper    2
-- Scisors  3

data Shape = None | Rock | Paper | Scisors deriving (Enum, Eq)

getShapes :: [String] -> [(Shape, Shape)]
getShapes = map getShapes
  where
    getShapes :: String -> (Shape, Shape)
    getShapes line = (getShape (head line), getShape (last line))
    getShape :: Char -> Shape
    getShape char
      | char `elem` ['A', 'X'] = toEnum 1
      | char `elem` ['B', 'Y'] = toEnum 2
      | otherwise = toEnum 3

getScore :: (Shape, Shape) -> Int
getScore (a, b)
  | a == b = 3 + fromEnum b
  -- Rock first
  | a == Rock && b == Paper = 6 + fromEnum b
  | a == Rock && b == Scisors = fromEnum b
  -- Paper first
  | a == Paper && b == Rock = fromEnum b
  | a == Paper && b == Scisors = 6 + fromEnum b
  -- Scisors First
  | a == Scisors && b == Rock = 6 + fromEnum b
  | a == Scisors && b == Paper = fromEnum b
  | otherwise = 1000000000000

main :: IO ()
main = do
  file <- openFile "input.txt" ReadMode
  content <- hGetContents file
  let fileLines = lines content

  putStr $ "The score is: " ++ show (sum . map getScore $ getShapes fileLines)
