import Control.Arrow
import qualified Data.Map.Strict as M

type BagSpec = (String, [(String, Int)])

type Rules = M.Map String (M.Map String Int)

(!) = (M.!)

parseLine :: String -> BagSpec
parseLine = driver [] . words
  where
    driver _ [] = undefined
    driver cols ("bags" : "contain" : rest) = (unwords cols, step2 [] Nothing rest)
    driver cols (color : rest) = driver (cols ++ [color]) rest

    step2 _ _ ["no", "other", "bags."] = []
    step2 cols Nothing (num : rest) = step2 cols (Just $ read num) rest
    step2 cols (Just num) ["bags."] = [(unwords cols, num)]
    step2 cols (Just num) ["bag."] = [(unwords cols, num)]
    step2 cols (Just num) ("bags," : rest) = (unwords cols, num) : step2 [] Nothing rest
    step2 cols (Just num) ("bag," : rest) = (unwords cols, num) : step2 [] Nothing rest
    step2 cols (Just num) (c : rest) = step2 (cols ++ [c]) (Just num) rest
    step2 _ _ _ = undefined

parse :: String -> Rules
parse = M.fromList . map (second M.fromList . parseLine) . lines

holdsGold :: Rules -> String -> Bool
holdsGold rules s =
  "shiny gold" `elem` relevant || any (holdsGold rules) relevant
  where
    relevant = M.keys $ rules ! s

solve1 :: String -> Int
solve1 s =
  length $ filter (holdsGold rules) $ M.keys rules
  where
    rules = parse s

mustContain :: Rules -> String -> Int
mustContain rules s =
  1 + (sum $ map (uncurry (*) . first (mustContain rules)) $ M.toList relevant)
  where
    relevant = rules ! s

solve2 :: String -> Int
solve2 s = mustContain (parse s) "shiny gold" - 1
