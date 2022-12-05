import Control.Arrow
import Data.Function
import Data.List
import Data.Map.Merge.Strict hiding (merge)
import qualified Data.Map.Merge.Strict as MM
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S

parse :: [String] -> M.Map String (S.Set String)
parse = foldr combine M.empty . map (go S.empty . words)
  where
    go _ [] = undefined
    go ingredients (s : ss) =
      case s of
        "(contains" ->
          -- every word here will have a trailing char, be it ',' or ')'
          let allergens = map init ss
           in foldr (flip M.insert ingredients) M.empty allergens
        _ -> go (S.insert s ingredients) ss

    combine =
      MM.merge
        preserveMissing
        preserveMissing
        (zipWithMatched (\_ -> S.intersection))

solveOne :: String -> Int
solveOne =
  length
    . uncurry filter
    . first potentialAllergen
    . (parse &&& allIngredients)
    . lines
  where
    allIngredients = concat . map (words . takeWhile (/= '('))
    potentialAllergen = flip S.notMember . M.foldr' S.union S.empty

solveConstraints :: M.Map String (S.Set String) -> M.Map String String
solveConstraints ms = go ms id (\_ -> undefined)
  where
    go ms s k =
      if M.null ms
        then s M.empty
        else
          let (allergen, candidates) =
                minimumBy (compare `on` (S.size . snd)) $ M.assocs ms
           in case S.minView candidates of
                Nothing -> k ()
                Just (guess, others) ->
                  let ms' = M.map (S.delete guess) $ M.delete allergen ms
                   in go
                        ms'
                        (s . M.insert allergen guess)
                        ( \() ->
                            let ms'' = M.insert allergen others
                             in go ms' s k
                        )

solveTwo :: String -> String
solveTwo =
  filter (not . (`elem` "[ \"]"))
    . show
    . M.elems
    . solveConstraints
    . parse
    . lines
