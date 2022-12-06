import Data.List

import System.IO (IOMode (ReadMode), hGetContents, openFile)

allUnique :: Eq a => [a] -> Bool
allUnique xs = nub xs == xs

findMarker :: Int -> String -> Int
findMarker count = go count
  where
    go n l@(x:xs) = if allUnique (take count l) then n else go (n+1) xs

part1 :: String -> Int
part1 = findMarker 4

part2 :: String -> Int
part2 = findMarker 14

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    input <- hGetContents handle
    print (part1 input)
    print (part2 input)
