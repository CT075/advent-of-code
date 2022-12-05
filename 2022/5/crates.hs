import Data.List

import System.IO (IOMode (ReadMode), hGetContents, openFile)

type Crate = Char
type Stack = [Crate]
type Stacks = [Stack]

data Instr = I
    { count :: Int
    , from :: Int
    , to :: Int
    }

type DoMove = Int -> Stack -> Stack -> (Stack, Stack)

update :: Int -> a -> [a] -> [a]
update 0 v (_ : xs) = v : xs
update n v (x : xs) = x : update (n - 1) v xs

move :: DoMove -> Int -> Int -> Int -> Stacks -> Stacks
move how n from to stacks =
    let s1 = stacks !! from
        s2 = stacks !! to
        (fromR, toR) = how n s1 s2
     in update from fromR $ update to toR stacks

initialStacks =
    [ "TRGWQMFP"
    , "RFH"
    , "DSHGVRZP"
    , "GWFBPHQ"
    , "HJMSP"
    , "LPRSHTZM"
    , "LMNHTP"
    , "RQDF"
    , "HPLNCSD"
    ]

runInstr :: DoMove -> Instr -> Stacks -> Stacks
runInstr how (I n from to) = move how n (from - 1) (to - 1)

moveSingle :: Stack -> Stack -> (Stack, Stack)
moveSingle (x : xs) ys = (xs, x : ys)

moveManual :: DoMove
moveManual n s1 s2 = iterate (uncurry moveSingle) (s1, s2) !! n

moveAuto :: DoMove
moveAuto n s1 s2 = (drop n s1, take n s1 ++ s2)

parse :: String -> [Instr]
parse = map (parseLine . words) . lines
  where
    parseLine :: [String] -> Instr
    parseLine ["move", n, "from", from, "to", to] =
        I
            { count = read n
            , from = read from
            , to = read to
            }
    parseLine s = error ("couldn't parse line:\n  " ++ unwords s)

part1 :: Stacks -> [Instr] -> String
part1 = curry $ map head . uncurry (foldl' (flip (runInstr moveManual)))

part2 :: Stacks -> [Instr] -> String
part2 = curry $ map head . uncurry (foldl' (flip (runInstr moveAuto)))

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let input = parse contents
    print (part1 initialStacks input)
    print (part2 initialStacks input)
