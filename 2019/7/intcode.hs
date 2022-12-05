import qualified Data.Vector as V
import Data.Char
import Control.Monad.State
import Data.List

data PrgState =
  P { pos :: Int
    , vec :: V.Vector Int
    , inStream :: [Int]
    , outStream :: [Int]
    }
  deriving Show

data Result b a = Ok a
                | Err b
                deriving Show

expose (Ok a) = a
expose (Err b) = error $ show b

(!) = (V.!)
(//) = (V.//)

next :: State PrgState Int
next = do
  v <- get
  let c = pos v
  put $ v { pos = c+1 }
  return $ (vec v) ! c

paramFromMode :: Int -> Int -> State PrgState Int
paramFromMode mode i = do
  v <- get
  case mode of
       0 -> return $ vec v ! i
       1 -> return $ i

update :: Int -> Int -> State PrgState ()
update dst vl = do
  v <- get
  put $ v { vec = vec v // [(dst, vl)] }

applyOp :: (Int -> Int -> Int) -> Int -> Int -> State PrgState ()
applyOp f p1Mode p2Mode = do
  p1 <- next >>= paramFromMode p1Mode
  p2 <- next >>= paramFromMode p2Mode
  dst <- next
  update dst $ f p1 p2

getInput :: State PrgState ()
getInput = do
  v <- get
  let (vl:rest) = inStream v
  dst <- next
  update dst vl
  v <- get
  put $ v { inStream = rest }

writeOut :: Int -> State PrgState ()
writeOut mode = do
  v <- get
  vl <- next >>= paramFromMode mode
  v <- get
  put $ v { outStream = (vl : outStream v) }

j :: (Int -> Bool) -> Int -> Int -> State PrgState ()
j p p1Mode p2Mode = do
  p1 <- next >>= paramFromMode p1Mode
  p2 <- next >>= paramFromMode p2Mode
  if p p1
     then do
       v <- get
       put $ v { pos = p2 }
     else
       return ()

cmp :: (Int -> Int -> Bool) -> Int -> Int -> State PrgState ()
cmp p p1Mode p2Mode = do
  p1 <- next >>= paramFromMode p1Mode
  p2 <- next >>= paramFromMode p2Mode
  dst <- next
  update dst $ if p p1 p2 then 1 else 0

run :: State PrgState (Result (Int, Int) [Int])
run = do
  i <- next
  let op = i `mod` 100
  let (1 : p3Mode : p2Mode : p1Mode : _) =
        map digitToInt $ show $ i + 100000
  case op of
       99 -> get >>= (return . Ok . reverse . outStream)
       1 -> runOnly $ applyOp (+) p1Mode p2Mode
       2 -> runOnly $ applyOp (*) p1Mode p2Mode
       3 -> runOnly $ getInput
       4 -> runOnly $ writeOut p1Mode
       5 -> runOnly $ j (/= 0) p1Mode p2Mode
       6 -> runOnly $ j (== 0) p1Mode p2Mode
       7 -> runOnly $ cmp (<) p1Mode p2Mode
       8 -> runOnly $ cmp (==) p1Mode p2Mode
       c -> do
         pos <- pos <$> get
         return $ Err (pos-1, c)
  where
    runOnly :: State PrgState () -> State PrgState (Result (Int,Int) [Int])
    runOnly = (>> run)

test v inp = runState run $ P 0 (V.fromList v) inp []

splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s =  case dropWhile p s of
                    "" -> []
                    s' -> w : splitOn p s''
                      where (w, s'') = break p s'

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let prg = map (read :: String -> Int) $ splitOn (==',') contents
  print $ show $
    maximum $
    map (runPrg prg) $ permutations [5,6,7,8,9]
  where
    runPrg prg [a,b,c,d,e] =
      let
        aOut = expose $ evalState run (P 0 (V.fromList prg) (a:0:eOut) [])
        bOut = expose $ evalState run (P 0 (V.fromList prg) (b:aOut) [])
        cOut = expose $ evalState run (P 0 (V.fromList prg) (c:bOut) [])
        dOut = expose $ evalState run (P 0 (V.fromList prg) (d:cOut) [])
        eOut = expose $ evalState run (P 0 (V.fromList prg) (e:dOut) [])
      in
      last eOut

