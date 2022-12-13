import Data.Char (digitToInt)
import Data.List.Split (splitOn)
import Debug.Trace (trace)

data Packet = IntPacket Int | PacketArray [Packet] deriving (Show)

parsePackets = (map . map) parseArray . packets
  where
    packets = map (take 2) . takeWhile (not . null) . iterate (drop 3) . lines

    parseArray :: String -> Packet
    parseArray arr
      | null arr = PacketArray []
      | '[' `notElem` arr = IntPacket $ digitToInt (head arr)
      | otherwise = PacketArray $ map parseArray elems ++ parsedOther
      where
        elems = (splitOn "," . takeWhile (/= '[') . init . tail) arr
        other = (dropWhile (/= '[') . init . tail) arr

        parsedOther = case other of
          "" -> []
          t -> [parseArray (trace ("\n" ++ show t) t)]

main :: IO ()
main = do
  content <- readFile "input.txt"

  print $ parsePackets content