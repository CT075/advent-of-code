import qualified Data.Map as M
import Data.Maybe

-- input: [0,13,16,17,1,10,6]

data Stored
  = Once Int
  | Twice Int Int

record :: Int -> Maybe Stored -> Stored
record currTurn Nothing = Once currTurn
record currTurn (Just (Once y)) = Twice currTurn y
record currTurn (Just (Twice y _)) = Twice currTurn y

initSt :: [(Int, Int)] -> M.Map Int Stored
initSt [] = M.empty
initSt ((x, currTurn) : xs) = M.alter (Just . record currTurn) x (initSt xs)

step :: M.Map Int Stored -> Int -> Int -> (M.Map Int Stored, Int)
step last currTurn prev =
  let result = case M.lookup prev last of
        Just (Twice x y) -> x - y
        _ -> 0
   in (M.alter (Just . record currTurn) result last, result)

solveOne xs n = snd $ go (length xs + 1) state (last xs)
  where
    state = initSt $ reverse $ zip xs [1 ..]

    go currTurn st prev =
      if currTurn > n
        then (st, prev)
        else
          let (st', curr) = step st currTurn prev
           in go (currTurn + 1) st' curr

-- this day sucks; "do that thing but bigger" is not an interesting problem
