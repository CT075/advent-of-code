import Control.Arrow
import Data.List
import qualified Data.Map.Strict as M

type Range = [(Int, Int)]

-- I can't be bothered to parse this
fields :: [(String, Range)]
fields =
  [ ("departure location", [(48, 793), (800, 971)]),
    ("departure station", [(36, 225), (247, 974)]),
    ("departure platform", [(25, 850), (862, 964)]),
    ("departure track", [(40, 173), (193, 959)]),
    ("departure date", [(42, 895), (902, 955)]),
    ("departure time", [(43, 692), (715, 951)]),
    ("arrival location", [(38, 528), (549, 967)]),
    ("arrival station", [(43, 133), (141, 963)]),
    ("arrival platform", [(40, 651), (675, 951)]),
    ("arrival track", [(48, 801), (811, 973)]),
    ("class", [(50, 562), (587, 955)]),
    ("duration", [(43, 520), (527, 968)]),
    ("price", [(44, 745), (752, 950)]),
    ("route", [(41, 929), (941, 963)]),
    ("row", [(37, 828), (838, 958)]),
    ("seat", [(47, 475), (491, 972)]),
    ("train", [(38, 198), (210, 965)]),
    ("type", [(33, 66), (74, 949)]),
    ("wagon", [(35, 492), (507, 962)]),
    ("zone", [(35, 358), (381, 965)])
  ]

myTicket =
  [ 157,
    59,
    163,
    149,
    83,
    131,
    107,
    89,
    109,
    113,
    151,
    53,
    127,
    97,
    79,
    103,
    101,
    173,
    167,
    61
  ]

invalidAll :: Int -> Bool
invalidAll n =
  all (all (\(x, y) -> n < x || y < n) . snd) $ fields

solveOne :: String -> Int
solveOne s =
  let numbers :: [[Int]]
      numbers = map (map read . words . map removeCommas) $ lines s
        where
          removeCommas ',' = ' '
          removeCommas x = x
   in sum $ filter invalidAll $ concat numbers

guessFields :: String -> [[String]]
guessFields s =
  let numbers :: [[Int]]
      numbers = map (map read . words . map removeCommas) $ lines s
        where
          removeCommas ',' = ' '
          removeCommas x = x

      validTickets :: [[Int]]
      validTickets = filter (not . any invalidAll) numbers

      positions :: [[Int]]
      positions = transpose validTickets

      allValidFields :: [Int] -> [String]
      allValidFields ns = map fst $ filter isValidField fields
        where
          isValidField (_, ranges) =
            all (\n -> any (\(x, y) -> x <= n && n <= y) ranges) ns
   in map allValidFields positions

assignFields :: [[String]] -> M.Map String Int
assignFields = go M.empty . zip [0 ..]
  where
    go :: M.Map String Int -> [(Int, [String])] -> M.Map String Int
    go assigned [] = assigned
    go assigned xs =
      let (singles, multiple) = partition ((== 1) . length . snd) xs
          singles' :: [(Int, String)]
          singles' = map (second head) singles
          assigned' :: M.Map String Int
          assigned' = foldr (uncurry $ flip M.insert) assigned singles'
          remaining = map (second $ filter (not . (`elem` map snd singles'))) $ multiple
       in go assigned' remaining

solveTwo :: String -> Int
solveTwo = multDepartures . assignFields . guessFields
  where
    (!) = (M.!)

    multDepartures ms =
      product $
        map
          ((myTicket !!) . (ms !))
          [ "departure location",
            "departure station",
            "departure platform",
            "departure track",
            "departure date",
            "departure time"
          ]
