import qualified Data.Map.Lazy as M
import Data.Maybe
import Control.Applicative

data Tree = Node String [Tree]
  deriving Show

splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s =  case dropWhile p s of
                    "" -> []
                    s' -> w : splitOn p s''
                      where (w, s'') = break p s'

splitOrbit :: String -> (String,String)
splitOrbit s =
  let [a,b] = splitOn (==')') s in
  (a,b)

construct :: [(String, String)] -> (Tree, String, String)
construct l = let
    (v, you, san) = foldl go (M.empty, "COM", "COM") l
  in
    (toTree "COM" v, you, san)
  where
    go (acc,a,b) (s1, s2) =
      case s2 of
           "YOU" -> (acc, s1, b)
           "SAN" -> (acc, a, s1)
           _ -> (M.alter (\v -> case v of
                                    Nothing -> Just [s2]
                                    Just ss -> Just (s2:ss)) s1 acc, a, b)

    toTree s m = Node s $ map (flip toTree m) $ M.findWithDefault [] s m

countOrbits :: M.Map String [String] -> Int
countOrbits m = go 0 "COM"
  where
    go :: Int -> String -> Int
    go i s =
      case M.lookup s m of
           Just ss -> i + (sum $ map (go(i+1)) ss)
           Nothing -> i

search :: (String -> Bool) -> Tree -> Maybe Int
search p (Node s children) =
  if p s then Just 0 else
  case catMaybes $ map (search p) children of
       [] -> Nothing
       xs -> Just $ 1 + sum xs

solve2 s1 s2 (t@(Node _ children)) =
  case catMaybes $ map (solve2 s1 s2) children of
       [] -> pathThrough t
       xs -> Just $ minimum xs
  where
    pathThrough t = liftA2 (+) (search (==s1) t) (search (==s2) t)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let orbits = map splitOrbit $ lines contents
  let (t, c1, c2) = construct orbits
  print $ c1
  print $ c2
  print $ show $ solve2 c1 c2 t

