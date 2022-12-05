import           Control.Arrow
import           Control.Monad
import qualified Data.Map.Strict as M
import           Data.Maybe

data Seat = Occupied | Empty deriving (Show, Eq)

data Board =
  B { table     :: M.Map (Int, Int) Seat
    , getWidth  :: Int
    , getHeight :: Int
    }
  deriving (Show, Eq)

newtype Direction = D (Int, Int)

(!?) = (M.!?)

display :: Board -> String
display board =
  concat
  [([disp (tbl !? (x,y)) | x <- [0..width-1]] ++ "\n") | y <- [0..height-1]]
  where tbl = table board
        width = getWidth board
        height = getHeight board

        disp Nothing         = '.'
        disp (Just Occupied) = '#'
        disp (Just Empty)    = 'L'

printBoard :: Board -> IO ()
printBoard = putStr . display

parse :: String -> Board
parse ss =
  let
    rows = lines ss
    height = length rows
    width = length $ head rows
    annotated = join $ zipWith (\y -> zipWith (\x s -> ((x,y), s)) [0..]) [0..] rows
    asList = map (second $ const Empty) $ filter ((== 'L') . snd) annotated
    tbl = M.fromList asList
  in
    B tbl width height

seesOccupied :: Board -> (Int, Int) -> Direction -> Bool
seesOccupied board (x,y) dir@(D (xOffs, yOffs)) =
    if x < 0 || x >= width || y < 0 || y >= height
       then False
       else case M.lookup next tbl of
                 Just Occupied -> True
                 Just Empty    -> False
                 Nothing       -> seesOccupied board next dir
  where tbl = table board
        width = getWidth board
        height = getHeight board
        next = (x+xOffs, y+yOffs)

step :: Board -> Board
step board = board { table = M.mapWithKey go tbl}
  where
    tbl = table board
    go loc status =
      let visible = length $ filter (seesOccupied board loc) directions
      in case status of
              Occupied -> if visible >= 5 then Empty else Occupied
              Empty    -> if visible == 0 then Occupied else Empty

directions :: [Direction]
directions = map D $ half ++ ((negate *** negate) <$> half)
  where half = [(1,0),(0,1),(1,1),(1,-1)]

fix :: Eq a => (a -> a) -> a -> a
fix f x = if x' == x then x' else fix f x'
  where x' = f x

readBoard :: String -> IO Board
readBoard = (parse <$>) . readFile

solve :: String -> IO ()
solve s = do
    board <- readBoard s
    let equilibrium = fix step board
    print $ M.foldr (+) 0 $ M.map numSeated $ table equilibrium
    return ()
  where numSeated Occupied = 1
        numSeated Empty    = 0

main :: IO ()
main = solve "input.txt"
