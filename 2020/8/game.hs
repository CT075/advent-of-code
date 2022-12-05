import           Data.List
import qualified Data.Map   as M
import           Data.Maybe
import qualified Data.Set   as S

data Command = Nop Int | Acc Int | Jmp Int deriving Show

data State =
  St
  { acc  :: Int
  , pc   :: Int
  , prog :: M.Map Int Command
  } deriving Show

(!?) = (M.!?)

parse :: String -> State
parse = St 0 0 . M.fromList . zip [0..] . map readCmd . lines
  where
    readCmd ('n':'o':'p':_:'+':rest) = Nop $ read rest
    readCmd ('n':'o':'p':_:rest)     = Nop $ read rest
    readCmd ('a':'c':'c':_:'+':rest) = Acc $ read rest
    readCmd ('a':'c':'c':_:rest)     = Acc $ read rest
    readCmd ('j':'m':'p':_:'+':rest) = Jmp $ read rest
    readCmd ('j':'m':'p':_:rest)     = Jmp $ read rest

runCmd :: State -> Command -> State
runCmd (St acc pc prog) cmd =
  case cmd of
       Nop _ -> St acc (pc+1) prog
       Acc i -> St (acc+i) (pc+1) prog
       Jmp i -> St acc (pc+i) prog

step :: State -> Maybe State
step st = runCmd st <$> prog st !? pc st

solve1 :: State -> Int
solve1 = go S.empty
  where
    go seen st =
      if pc st `S.member` seen
         then acc st
         else go (S.insert (pc st) seen) $ fromJust $ step st

mods :: State -> [State]
mods st =
    St 0 0 <$> (map M.fromList $ flipOne $ M.toList $ prog st)
  where
    flipOne [] = []
    flipOne (v@(idx,Nop i):rest) = ((idx,Jmp i):rest) : map (v:) (flipOne rest)
    flipOne (v@(idx,Jmp i):rest) = ((idx,Nop i):rest) : map (v:) (flipOne rest)
    flipOne (oth:rest) = map (oth:) $ flipOne rest

run :: State -> Maybe Int
run = go S.empty
  where
    go seen st =
      if pc st `S.member` seen
         then Nothing
         else case step st of
                   Nothing  -> Just $ acc st
                   Just st' -> go (S.insert (pc st) seen) st'

solve2 :: State -> Maybe Int
solve2 = foldl' findJust Nothing . map run . mods
  where
    findJust (Just a) _      = Just a
    findJust _ (Just a)      = Just a
    findJust Nothing Nothing = Nothing
