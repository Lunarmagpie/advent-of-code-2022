import Data.List (isPrefixOf, isSubsequenceOf, uncons)
import Data.Map (Map, empty, insert, (!))
import qualified Data.Map (map)
import Debug.Trace (trace)

data Directory = File Int | Folder (Map String Directory) deriving (Show)

directorySize :: Directory -> Int
directorySize (Folder dir) = sum $ Data.Map.map directorySize dir
directorySize (File size) = size

directorySizes :: Directory -> [Int]
directorySizes (Folder dir) = directorySize (Folder dir) : concat (Data.Map.map directorySizes dir)
directorySizes (File size) = []

firstIndexWhere :: (a -> Bool) -> [a] -> Int
firstIndexWhere func items = inner func items 0
  where
    inner :: (a -> Bool) -> [a] -> Int -> Int
    inner f (x : xs) n = if f x then n else inner f xs n + 1
    inner _ _ n = n + 1

splitCommands :: [String] -> [[String]]
splitCommands [] = []
splitCommands lines = take until lines : splitCommands (drop until lines)
  where
    until = firstIndexWhere (\a -> "$" `isPrefixOf` a) (drop 1 lines) + 1

processCD :: ([String], Directory) -> [String] -> ([String], Directory)
processCD (workingDir, dir) command = (newDir toDir, dir)
  where
    newDir d
      | d == ".." = (reverse . drop 1 . reverse) workingDir
      | d == "/" = []
      | otherwise = workingDir ++ [d]
    toDir = words command' !! 2
    command' = head command

processLS :: ([String], Directory) -> [String] -> ([String], Directory)
processLS (workingPath, directory) output = (workingPath, foldl lambda directory files)
  where
    files :: [(String, Directory)]
    files = map toDirectory (drop 1 output)

    toDirectory :: String -> (String, Directory)
    toDirectory x
      | head (words x) == "dir" = (words x !! 1, Folder empty)
      | otherwise = (words x !! 1, File $ read $ head (words x))

    lambda :: Directory -> (String, Directory) -> Directory
    lambda directory (filename, file) = insertDir directory workingPath filename file

insertDir :: Directory -> [String] -> String -> Directory -> Directory
insertDir tree (path : rest) fileanme newDir = case tree of
  Folder x -> Folder $ insert path (insertDir (x ! path) rest fileanme newDir) x
  File x -> File x
insertDir (Folder tree) [] filename newDir = Folder (insert filename newDir tree)
insertDir (File tree) [] filename newDir = File tree

getDir :: Directory -> [String] -> Directory
getDir d [] = d
getDir d (x : xs) = case d of
  Folder folder -> getDir (folder ! x) xs
  File size -> error "hmmm interesting"

processCommands :: [String] -> ([String], Directory)
processCommands fileLines = foldl processAnyCommand ([], Folder empty) (splitCommands fileLines)
  where
    processAnyCommand :: ([String], Directory) -> [String] -> ([String], Directory)
    processAnyCommand a b = if "$ cd" `isSubsequenceOf` head b then processCD a b else processLS a b

    commands = splitCommands fileLines

totalUnder100000 :: [Int] -> Int
totalUnder100000 = sum . filter (< 100000)

smallestToDelete :: [Int] -> Int -> Int
smallestToDelete sizes totalSize = minimum $ filter (> spaceNeeded) sizes
  where
    freeSpace = 70000000 - totalSize
    spaceNeeded = 30000000 - freeSpace

main :: IO ()
main = do
  content <- readFile "input.txt"
  let fileLines = lines content

  let (_, dir) = processCommands fileLines

  print $ totalUnder100000 $ directorySizes dir
  print $ smallestToDelete (directorySizes dir) (directorySize dir)
