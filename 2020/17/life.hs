import Control.Arrow
import Control.Monad
import Data.Maybe
import qualified Data.Set as S

type Point = (Int, Int, Int)

type Board = S.Set Point

member :: Ord a => a -> S.Set a -> Bool
member = S.member

parse :: String -> Board
parse s =
  let grid = lines s
      annotated =
        map (\(y, (x, v)) -> ((x, y), v)) $
          join $
            zipWith (map . (,)) [0 ..] $ map (zip [0 ..]) grid
      positions =
        foldr
          (\((x, y), v) -> if v == '#' then S.insert (x, y, 0) else id)
          S.empty
          annotated
   in positions

adjacents :: [(Int, Int, Int)]
adjacents =
  [ (x, y, z)
    | x <- [-1, 0, 1],
      y <- [-1, 0, 1],
      z <- [-1, 0, 1],
      (x, y, z) /= (0, 0, 0)
  ]

applyDir :: Point -> (Int, Int, Int) -> Point
applyDir (x, y, z) (i, j, k) = (x + i, y + j, z + k)

makeView :: Point -> [Point]
makeView p = map (applyDir p) adjacents

step :: Board -> Board
step = uncurry (flip foldr S.empty) . (adjustCells &&& id)
  where
    adjustCells :: Board -> Point -> S.Set Point -> S.Set Point
    adjustCells b p s =
      let liveInView = S.size . S.intersection b
          pointsViewed = makeView p
          s' =
            if (liveInView $ S.fromList pointsViewed) `elem` [2, 3]
              then S.insert p s
              else s

          alsoAlive p' =
            -- don't need to handle this case here, it will be checked anyway
            not (p' `member` b)
              && (liveInView $ S.fromList $ makeView p') == 3

          newLiveAdj = S.fromList $ filter alsoAlive pointsViewed
       in S.union s' newLiveAdj

solveOne :: String -> Int
solveOne = S.size . step . step . step . step . step . step . parse

type Point2 = (Int, Int, Int, Int)

type Board2 = S.Set Point2

parseTwo :: String -> Board2
parseTwo = S.map (\(x, y, z) -> (x, y, z, 0)) . parse

-- properly we should be able to reuse the same code via typeclasses, but I'm
-- lazy
adjacents2 :: [(Int, Int, Int, Int)]
adjacents2 =
  [ (x, y, z, w)
    | x <- [-1, 0, 1],
      y <- [-1, 0, 1],
      z <- [-1, 0, 1],
      w <- [-1, 0, 1],
      (x, y, z, w) /= (0, 0, 0, 0)
  ]

applyDir2 :: Point2 -> (Int, Int, Int, Int) -> Point2
applyDir2 (x, y, z, w) (i, j, k, l) = (x + i, y + j, z + k, w + l)

makeView2 :: Point2 -> [Point2]
makeView2 p = map (applyDir2 p) adjacents2

step2 :: Board2 -> Board2
step2 = uncurry (flip foldr S.empty) . (adjustCells &&& id)
  where
    adjustCells :: Board2 -> Point2 -> S.Set Point2 -> S.Set Point2
    adjustCells b p s =
      let liveInView = S.size . S.intersection b
          pointsViewed = makeView2 p
          s' =
            if (liveInView $ S.fromList pointsViewed) `elem` [2, 3]
              then S.insert p s
              else s

          alsoAlive p' =
            -- don't need to handle this case here, it will be checked anyway
            not (p' `member` b)
              && (liveInView $ S.fromList $ makeView2 p') == 3

          newLiveAdj = S.fromList $ filter alsoAlive pointsViewed
       in S.union s' newLiveAdj

solveTwo :: String -> Int
solveTwo = S.size . step2 . step2 . step2 . step2 . step2 . step2 . parseTwo
