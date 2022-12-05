import           Control.Arrow
import           Data.List
import           Prelude       hiding (Left, Right)

data Direction = North | East | South | West
                 deriving (Eq, Show)

data Turn = Left | Right

data Command = N Int | S Int | E Int | W Int | L Int | R Int | F Int
             deriving (Read, Show)

data State =
  St { facing   :: Direction
     , position :: (Int, Int)
     , waypoint :: (Int, Int)
     }
  deriving Show

adjust :: String -> String
adjust []     = undefined
adjust (c:cs) = c:' ':cs

directions :: [Direction]
directions = [North, East, South, West]

move :: Direction -> Int -> (Int, Int) -> (Int, Int)
move North = second . (+)
move South = second . (subtract)
move East  = first . (+)
move West  = first . (subtract)

turn :: Turn -> Int -> Direction -> Direction
turn t i dir =
    directions !! ((4 + ((dirpos dir + i * diroffs t) `mod` 4)) `mod` 4)
  where dirpos North = 4
        dirpos East  = 1
        dirpos South = 2
        dirpos West  = 3

        diroffs Left  = -1
        diroffs Right = 1

drive :: State -> Command -> State
drive st cmd =
  case cmd of
       (N i) -> st { position = move North i pos }
       (E i) -> st { position = move East i pos }
       (W i) -> st { position = move West i pos }
       (S i) -> st { position = move South i pos }
       (F i) -> st { position = move dir i pos }
       (R i) -> st { facing = turn Right (i `div` 90) dir }
       (L i) -> st { facing = turn Left (i `div` 90) dir }
  where pos = position st
        dir = facing st

rotate :: Turn -> Int -> (Int, Int) -> (Int, Int)
rotate dir i way = iterate (single dir) way !! i
  where
    single Left (x,y)  = (-y, x)
    single Right (x,y) = (y, -x)

actual_drive :: State -> Command -> State
actual_drive st cmd =
  case cmd of
       (F i) -> st { position = (x + xoffs*i, y + yoffs*i) }
       (N i) -> st { waypoint = move North i way }
       (E i) -> st { waypoint = move East i way }
       (W i) -> st { waypoint = move West i way }
       (S i) -> st { waypoint = move South i way }
       (R i) -> st { waypoint = rotate Right (i `div` 90) way }
       (L i) -> st { waypoint = rotate Left (i `div` 90) way }
  where (x,y) = position st
        dir = facing st
        way :: (Int, Int)
        way@(xoffs, yoffs) = waypoint st

main :: IO ()
main = do
  directions <- lines <$> readFile "input.txt"
  let dirs = ((read . adjust) <$> directions) :: [Command]
  let part1 = foldl' drive (St East (0,0) (0,0)) dirs
  print $ part1
  print $ uncurry (+) $ (abs *** abs) $ position part1
  let part2 = foldl' actual_drive (St East (0,0) (10,1)) dirs
  print $ part2
  print $ uncurry (+) $ (abs *** abs) $ position part2
