
import Control.Arrow
import Data.Maybe
import Data.List

data Dir = U
         | R
         | D
         | L
         deriving (Show)

type Point = (Int, Int)

data Lines = P { horz :: [(Int, Point, Point)]
               , vert :: [(Int, Point, Point)]
               }
             deriving Show

coordTransform :: Dir -> Int -> (Int, Int) -> (Int, Int)
coordTransform U = second . (+)
coordTransform R = first . (+)
coordTransform D = second . subtract
coordTransform L = first . subtract

isHorz :: Dir -> Bool
isHorz U = False
isHorz D = False
isHorz L = True
isHorz R = True

populateWire :: [(Dir, Int)] -> Lines
populateWire = go 0 (0,0)
  where go curr pos [] = P [] []
        go curr pos (d@(dir, dist):rest) =
          let pos' = uncurry coordTransform d pos
              rec = go (curr+dist) pos' rest
          in
          if isHorz dir
             then rec { horz = (curr, pos, pos') : horz rec }
             else rec { vert = (curr, pos, pos') : vert rec }

solve1 :: Lines -> [(Dir, Int)] -> Point
solve1 ls =
  minimumBy (curry $ uncurry compare . (mdist *** mdist)) .
  concat . go (0,0)
  where
    mdist :: Point -> Int
    mdist (a,b) = abs a + abs b

    intersections dir ((x1, y1), (x2, y2)) =
      if isHorz dir
         then catMaybes $ map vIntersection $ vert ls
         else catMaybes $ map hIntersection $ horz ls
      where
        vIntersection (_,(x',y1'),(_,y2')) =
          if min x1 x2 <= x' && x' <= max x1 x2 &&
             min y1' y2' <= y1 && y1 <= max y1' y2'
             then Just (x',y1) else Nothing
        hIntersection (_,(x1',y'),(x2',_)) =
          if min y1 y2 <= y' && y' <= max y1 y2 &&
             min x1' x2' <= x1 && x1 <= max x1' x2'
             then Just (x1,y') else Nothing

    go pos [] = []
    go pos (d@(dir,_):rest) =
      let pos' = uncurry coordTransform d pos in
         intersections dir (pos, pos') : go pos' rest

solve2 ls =
  minimum .
  map (\(a,b,c) -> a+b) .
  concat . go 0 (0,0)
  where
    intersections :: Dir -> (Point, Point) -> [(Int, Int, Point)]
    intersections dir ((x1, y1), (x2, y2)) =
      if isHorz dir
         then catMaybes $ map vIntersection $ vert ls
         else catMaybes $ map hIntersection $ horz ls
      where
        vIntersection (dist, (x',y1'),(_,y2')) =
          if min x1 x2 <= x' && x' <= max x1 x2 &&
             min y1' y2' <= y1 && y1 <= max y1' y2'
             then Just ((dist+ abs (y1'-y1)),(abs $ x'-x1), (x',y1))
             else Nothing
        hIntersection (dist, (x1',y'),(x2',_)) =
          if min y1 y2 <= y' && y' <= max y1 y2 &&
             min x1' x2' <= x1 && x1 <= max x1' x2'
             then Just ((dist + abs (x1'-x1)), (abs $ y'-y1),(x1,y'))
             else Nothing

    go :: Int -> Point -> [(Dir, Int)] -> [[(Int, Int, Point)]]
    go curr pos [] = []
    go curr pos (d@(dir,dist):rest) =
      let pos' = uncurry coordTransform d pos in
         map (\(a,b,c) -> (a,b+curr,c)) (intersections dir (pos, pos')) :
         go (curr+dist) pos' rest

splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s =  case dropWhile p s of
                    "" -> []
                    s' -> w : splitOn p s''
                      where (w, s'') = break p s'

parseDir :: String -> [(Dir, Int)]
parseDir = map parseSingle . splitOn (==',')
  where
    parseSingle ('U':rest) = (U, read rest)
    parseSingle ('R':rest) = (R, read rest)
    parseSingle ('D':rest) = (D, read rest)
    parseSingle ('L':rest) = (L, read rest)
    parseSingle _ = error "impossible!"

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let [fst, snd] = lines contents
  let wire1 = populateWire $ parseDir fst
  let wire2 = parseDir snd
  print $ solve1 wire1 wire2
  print $ solve2 wire1 wire2
  return ()

