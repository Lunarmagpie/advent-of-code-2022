import Data.Char (digitToInt)
import Data.List (isPrefixOf)
import Data.List.Split (splitOn, startsWith)
import Debug.Trace (trace)

data Packet = IntPacket String | PacketArray [Packet] | Unparsed String deriving (Show)

parsePackets = (map . map) parseArray . packets
  where
    packets = map (take 2) . takeWhile (not . null) . iterate (drop 3) . lines

    parseArray :: String -> Packet
    parseArray packet
      | packet == "[]" = PacketArray []
      | "[" `isPrefixOf` packet = (parseArray . init . tail) packet
      -- Must be a list of numbers
      | otherwise = (PacketArray . map (IntPacket) . splitOn ",") packet


-- takeUntilLevel :: String -> String
-- takeUntilLevel s = inner s 0
--   where
--     inner :: String -> Int -> String
--     inner ('[' : xs) 0 = ['[']
--     inner (']' : xs) 0 = [']']
--     inner ('[' : xs) a = '[' : inner xs (a + 1)
--     inner (']' : xs) a = ']' : inner xs (a - 1)
--     inner (x : xs) a = x : inner xs a
--     inner [] _ = []

main :: IO ()
main = do
  content <- readFile "input.txt"

  print $ parsePackets content

  -- print $ (read "[1,2,3,4]" :: [Int])
