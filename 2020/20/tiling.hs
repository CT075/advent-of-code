import Control.Arrow
import Control.Monad
import Data.Function
import Data.List
import Data.List.Split
import Data.Maybe

-- We could use [M.Map (Int, Int) Bool] here, but I don't think it's really
-- necessary, as the tiles are relatively small.
type Tile = [[Bool]]

parse :: String -> [(Integer, Tile)]
parse = map (parseTile . lines) . splitOn "\n\n"
  where
    parseTile [] = undefined
    parseTile (number : rest) =
      let n :: Integer
          n = read $ init $ head $ tail $ words number

          grid = map (map (== '#')) rest
       in (n, grid)

edges :: [[a]] -> [[a]]
edges =
  uncurry (++)
    . (id &&& map reverse)
    . flip map [head, last, map head, map last]
    . flip ($)

solveOne :: String -> Integer
solveOne = product . take 4 . go . map (second edges) . parse
  where
    go [] = undefined
    go ((n, es) : rest) =
      if null $ drop 2 $ filter (any (`elem` es)) $ map snd rest
        then n : go (rest ++ [(n, es)])
        else go (rest ++ [(n, es)])

tryMerge :: Tile -> Tile -> [Tile]
tryMerge g1 g2 =
  [ reverse (tail g1') ++ tail g2''
    | g1' <- [g1, reverse g1],
      g2' <- [g2, reverse g2],
      g2'' <- take 4 $ iterate (transpose . reverse) g2',
      head g1' == head g2''
  ]

mergeCol :: [Tile] -> [Tile]
mergeCol (grid : grids) = go [] grids
  where
    go gs [] = reverse gs ++ [grid]
    go gs' (g : gs) =
      case tryMerge grid g of
        (g' : _) -> mergeCol (g' : (reverse gs' ++ gs))
        [] -> go (g : gs') gs

removeBorders :: Tile -> Tile
removeBorders = map (init . tail) . init . tail

stitch :: [Tile] -> Tile
stitch [grid] = removeBorders grid
stitch grids =
  stitch $ map transpose $ repeatUntil ((==) `on` length) mergeCol grids
  where
    repeatUntil pred f xs =
      let ys = f xs
       in if pred xs ys then ys else repeatUntil pred f ys

data Compact = E | F deriving (Eq)

compress :: Tile -> [[Compact]]
compress = map $ map toChar
  where
    toChar True = F
    toChar False = E

findMonstersH :: [[Compact]] -> Integer
findMonstersH ([] : _) = 0
findMonstersH [] = 0
-- thanks, I hate it
{- ORMOLU_DISABLE -}
findMonstersH g@(
  (_ : _ : _ : _ : _ : _ : _ : _ : _ : _ : _ : _ : _ : _ : _ : _ : _ : _ : F : _ : _) :
  (F : _ : _ : _ : _ : F : F : _ : _ : _ : _ : F : F : _ : _ : _ : _ : F : F : F : _) :
  (_ : F : _ : _ : F : _ : _ : F : _ : _ : F : _ : _ : F : _ : _ : F : _ : _ : _ : _) :
   _) =
  (+ 1) $ findMonstersH $ map tail g
{- ORMOLU_ENABLE -}
findMonstersH g = findMonstersH $ map tail g

findMonstersV :: [[Compact]] -> Integer
findMonstersV = sum . map findMonstersH . tails

countBest :: [[Compact]] -> Integer
countBest g =
  maximum
    [ findMonstersV g''
      | g' <- [g, reverse g],
        g'' <- take 4 $ iterate (transpose . reverse) g'
    ]

sea :: [[Compact]] -> Integer
sea =
  uncurry (-) . ((toInteger . length . filter (== F) . concat) &&& ((* 15) . countBest))

solveTwo :: String -> Integer
solveTwo = sea . compress . stitch . map snd . parse
