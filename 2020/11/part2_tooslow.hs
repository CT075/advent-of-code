import           Control.Arrow
import qualified Data.List     as L
import           Data.Maybe
import           Data.Vector
import           Prelude       hiding (concat, filter, head, length, map,
                                takeWhile, (++))

data Seat = Occupied | Empty deriving (Show, Eq)

display :: Vector (Vector (Maybe Seat)) -> String
display board = toList $ concat $ toList $ fmap (fmap disp) board
  where disp Nothing         = '.'
        disp (Just Occupied) = '#'
        disp (Just Empty)    = 'L'

status :: Char -> Maybe Seat
status '.' = Nothing
status 'L' = Just Empty
status _   = undefined

parse :: String -> Vector (Vector (Maybe Seat))
parse = fromList . fmap fromList . ((status <$>) <$>) . lines

directions :: [(Int, Int)]
directions = half ++ ((negate *** negate) <$> half)
  where half = [(1,0),(0,1),(1,1),(1,-1)]
        (++) = (L.++)

seatVisible :: Vector (Vector (Maybe Seat)) -> (Int, Int) -> (Int, Int) -> Bool
seatVisible board (x,y) (xOffs, yOffs)  =
    fromMaybe False
      $ foldl' f Nothing
      $ map (fmap isOccupied . uncurry index)
      $ takeWhile (\(x,y) -> 0 <= x && x < width && 0 <= y && y < height)
      $ fromList $ (((x+) . (xOffs*)) &&& ((y+) . (yOffs*))) <$> [1..]
  where
    -- index x y = (board ! y) ! x
    index = flip (.) (board !) . flip (!)
    height = length board
    width = length (head board)

    isOccupied  Occupied = True
    isOccupied Empty     = False

    f (Just x) (Just y) = Just x
    f (Just x) Nothing  = Just x
    f Nothing (Just y)  = Just y
    f Nothing Nothing   = Nothing

step :: Vector (Vector (Maybe Seat)) -> Vector (Vector (Maybe Seat))
step board =
    fmap (uncurry stepSeat <$>) $
    generate height (\y -> generate width (\x -> (x,y)))
  where
    index = flip (.) (board !) . flip (!)
    height = length board
    width = length (head board)

    seatChange visible Occupied = if visible > 4 then Empty else Occupied
    seatChange visible Empty    = if visible == 0 then Occupied else Empty

    stepSeat :: Int -> Int -> Maybe Seat
    stepSeat x y =
      let visible = L.length $ L.filter (seatVisible board (x,y)) directions
      in seatChange visible <$> index x y

fix :: Eq a => (a -> a) -> a -> a
fix f x = if x' == x then x' else fix f x'
  where x' = f x

readBoard :: String -> IO (Vector (Vector (Maybe Seat)))
readBoard = (parse <$>) . readFile

solve :: String -> IO ()
solve s = do
    board <- readBoard s
    let equilibrium = fix step board
    print $ length $ filter isOccupied $ concat $ toList $ equilibrium
    return ()
  where
    isOccupied (Just Occupied) = True
    isOccupied _               = False

main :: IO ()
main = solve "input.txt"
