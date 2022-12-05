import Data.List hiding (iterate')
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S

type HexCoord = (Int, Int, Int)

data HexGrid = H {unH :: M.Map HexCoord Bool}

ne (x, y, z) = (x + 1, y, z -1)

nw (x, y, z) = (x, y + 1, z -1)

se (x, y, z) = (x, y -1, z + 1)

sw (x, y, z) = (x -1, y, z + 1)

e (x, y, z) = (x + 1, y -1, z)

w (x, y, z) = (x -1, y + 1, z)

flipTile :: Maybe Bool -> Bool
flipTile = not . fromMaybe False

followPath :: HexCoord -> String -> HexCoord
followPath pt [] = pt
followPath pt ('n' : 'e' : rest) = followPath (ne pt) rest
followPath pt ('n' : 'w' : rest) = followPath (nw pt) rest
followPath pt ('s' : 'e' : rest) = followPath (se pt) rest
followPath pt ('s' : 'w' : rest) = followPath (sw pt) rest
followPath pt ('e' : rest) = followPath (e pt) rest
followPath pt ('w' : rest) = followPath (w pt) rest
followPath _ _ = undefined

runInstructions :: HexGrid -> [String] -> HexGrid
runInstructions = foldl' runSingle
  where
    runSingle (H grid) s = H (M.alter (Just . flipTile) (followPath (0, 0, 0) s) grid)

solveOne :: String -> Int
solveOne =
  M.foldl' (\acc x -> if x then acc + 1 else acc) 0
    . unH
    . runInstructions (H M.empty)
    . lines

type Tiles = S.Set HexCoord

neighbors :: HexCoord -> S.Set HexCoord
neighbors = S.fromList . flip map [ne, nw, se, sw, e, w] . flip ($)

member = S.member

step :: Tiles -> Tiles
step s =
  let liveAdjacent :: HexCoord -> Tiles -> Int
      liveAdjacent pt s = S.size $ S.intersection s $ neighbors pt

      stillLive :: HexCoord -> Tiles -> Bool
      stillLive pt grid =
        not (liveAdjacent pt grid == 0 || liveAdjacent pt grid > 2)

      becomeLive :: HexCoord -> Tiles -> Bool
      becomeLive pt grid = liveAdjacent pt grid == 2

      pointsToConsider :: S.Set HexCoord
      pointsToConsider = (S.unions $ map neighbors $ S.toList s)

      maybeFlip :: Tiles -> HexCoord -> Tiles
      maybeFlip s' pt = if checkLive pt s then S.insert pt s' else s'
        where
          checkLive = if pt `member` s then stillLive else becomeLive
   in S.foldl maybeFlip S.empty pointsToConsider

iterate' 0 _ x = x
iterate' n f x = x `seq` iterate' (n -1) f (f x)

solveTwo :: String -> Int
solveTwo =
  S.size
    . iterate' 100 step
    . S.fromList
    . M.keys
    . M.filter id
    . unH
    . runInstructions (H M.empty)
    . lines
