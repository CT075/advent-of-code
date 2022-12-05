import Control.Arrow
import Data.List
import qualified Data.Map as M

step :: Int -> [Int] -> [Int]
step _ [] = undefined
step biggest (x : xs) =
  let numbersDecreasing = cycle $ reverse [1 .. biggest]
      (picked, inPlay) = splitAt 3 xs
      x' = if x == 1 then biggest + 1 else x
      chosen =
        head $
          dropWhile (`elem` picked) $
            dropWhile (>= x') $
              numbersDecreasing
      (prefix, _ : rest) = span (/= chosen) inPlay
   in drop 1 $ cycle $ take 9 $ x : prefix ++ chosen : picked ++ rest

runN :: Int -> Int -> [Int] -> [Int]
runN = flip (.) (flip (!!)) . flip (.) . iterate . step

solveOne :: String -> String
solveOne =
  concat
    . map show
    . take 8
    . drop 1
    . dropWhile (/= 1)
    . runN 9 100
    . cycle
    . map (read . (: []))

data Circle = C
  { getCursor :: Int,
    getLargest :: Int,
    getNextMap :: M.Map Int Int
  }
  deriving (Show)

(!) = (M.!)

stepC :: Circle -> Circle
stepC (C cursor largest nexts) =
  let picked = take 3 $ drop 1 $ iterate (nexts !) cursor
      dst = selectTarget cursor picked
      nexts' = M.insert cursor (nexts ! last picked) nexts
      nexts'' = M.insert dst (head picked) nexts'
      nexts''' = M.insert (last picked) (nexts ! dst) nexts''
   in C (nexts''' ! cursor) largest nexts'''
  where
    selectTarget c ineligible =
      let c' = if c == 1 then largest else c - 1
       in if c' `elem` ineligible
            then selectTarget c' ineligible
            else c'

buildAdjCyclic :: [Int] -> M.Map Int Int
buildAdjCyclic xs = M.insert (last xs) (head xs) $ M.fromList (zip xs (tail xs))

buildCircle :: Int -> [Int] -> Circle
buildCircle max xs =
  let adj = buildAdjCyclic xs
      maxIn = maximum xs
      adj' = foldl' (flip (uncurry M.insert . (id &&& (+ 1)))) adj [maxIn + 1 .. max -1]
      adj'' = if max > maxIn then M.insert max (head xs) adj' else adj'
      adj''' = if max > maxIn then M.insert (last xs) (maxIn + 1) adj'' else adj''
   in C (head xs) max adj'''

findStars :: [Int] -> Int
findStars xs =
  let circ = buildCircle 1000000 xs
      C _ _ endState = iterate stepC circ !! 10000000
   in (endState ! 1) * (endState ! (endState ! 1))
