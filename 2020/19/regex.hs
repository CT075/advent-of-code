import Control.Arrow
import qualified Data.Map as M

data Rule
  = Const Char
  | Alt Rule Rule
  | Seq [Int]
  deriving (Show)

(!) = (M.!)

parseRule :: String -> (Int, Rule)
parseRule s =
  case words s of
    [] -> undefined
    num : rest -> (read $ filter (/= ':') num, buildRule [] rest)
  where
    buildRule acc [] = Seq (reverse acc)
    buildRule acc ("|" : rest) = Alt (Seq (reverse acc)) (buildRule [] rest)
    -- we can cheat here, we know that the leaves are always single characters
    buildRule acc [['"', c, '"']] = Const c
    buildRule acc (n : rest) = buildRule (read n : acc) rest

buildRules :: [String] -> M.Map Int Rule
buildRules = M.fromList . map parseRule

match :: M.Map Int Rule -> Rule -> String -> (String -> Bool) -> Bool
match _ (Const c) (x : xs) k = if x == c then k xs else False
match rules (Const c) [] _ = False
match rules (Seq (r : rs)) xs k =
  match rules (rules ! r) xs (flip (match rules (Seq rs)) k)
match _ (Seq []) xs k = k xs
match rules (Alt r1 r2) xs k = match rules r1 xs k || match rules r2 xs k

match' :: M.Map Int Rule -> Rule -> String -> Bool
match' rules r s = match rules r s null

solveOne :: String -> Int
solveOne s =
  let (ruleLines, inputs) = second tail $ span (/= "") $ lines s
      rules = buildRules ruleLines
   in length $ filter (match' rules (Seq [0])) inputs

new8 :: Rule
new8 = Alt (Seq [42]) (Seq [42, 8])

new11 :: Rule
new11 = Alt (Seq [42, 31]) (Seq [42, 11, 31])

solveTwo :: String -> Int
solveTwo s =
  let (ruleLines, inputs) = second tail $ span (/= "") $ lines s
      rules = M.insert 8 new8 $ M.insert 11 new11 $ buildRules ruleLines
   in length $ filter (match' rules (Seq [0])) inputs
