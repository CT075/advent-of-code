import Prelude hiding (head)
import Data.Vector
import Data.Csv
import Control.Monad.State

data PrgState =
  P { pos :: Int
    , vec :: Vector Int
    }
  deriving Show

run :: State PrgState (Maybe Int)
run = do
  v <- get
  let c = pos v
  case vec v ! c of
       99 -> return $ Just $ head $ vec v
       1 -> applyOp (+) (vec v) c
       2 -> applyOp (*) (vec v) c
       _ -> return Nothing
  where
    applyOp f v index = do
      let srcl = v ! (index+1)
      let srcr = v ! (index+2)
      let l = v ! srcl
      let r = v ! srcr
      let dst = v ! (index+3)
      let v' = v // [(dst, f l r)]
      put $ P (index+4) v'
      run

answer :: Vector Int -> Int -> Maybe (Int, Int)
answer v target = go 0 0
  where
    go x y =
      if x >= 100 then go 0 (y+1) else
      if y >= 100 then Nothing else
      case fst $ runState run (P 0 $ v // [(1, x), (2, y)]) of
           Just n -> if n == target then Just (x,y) else go (x+1) y
           Nothing -> go (x+1) y

