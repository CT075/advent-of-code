import Control.Applicative
import Data.List
import qualified Data.Map.Strict as M

data Bit = Zero | One deriving (Show)

type Mask = [Maybe Bit]

data State = St
  { getMemory :: M.Map Integer [Bit],
    getMask :: Mask
  }

parseMask :: String -> Mask
parseMask = map indiv
  where
    indiv 'X' = Nothing
    indiv '1' = Just One
    indiv '0' = Just Zero

applyMask :: Mask -> [Bit] -> [Bit]
applyMask = zipWith maskBit
  where
    maskBit Nothing x = x
    maskBit (Just y) _ = y

intToBits :: Integer -> [Bit]
intToBits = leftpad 36 . reverse . go
  where
    go 0 = []
    go n = (if n `mod` 2 == 1 then One else Zero) : go (n `div` 2)

    leftpad m xs = replicate (m - length xs) Zero ++ xs

bitsToInt :: [Bit] -> Integer
bitsToInt = go . reverse
  where
    go [] = 0
    go (Zero : xs) = 2 * go xs
    go (One : xs) = 1 + 2 * go xs

runInstr :: State -> String -> State
runInstr (St mem mask) s =
  case words s of
    "mask" : "=" : rest -> St mem (parseMask $ concat rest)
    ('m' : 'e' : 'm' : '[' : num) : "=" : rest ->
      let index = read $ take (length num - 1) num
          n = intToBits $ read $ concat $ rest
          mem' = M.insert index (applyMask mask n) mem
       in St mem' mask
    _ -> undefined

solveOne :: String -> Integer
solveOne =
  M.foldr ((+) . bitsToInt) 0
    . getMemory
    . foldl runInstr (St M.empty (replicate 36 Nothing))
    . lines

applyMaskV2 :: Mask -> [Bit] -> [Integer]
applyMaskV2 m = map bitsToInt . foldr (liftA2 (:)) [[]] . zipWith maskBit m
  where
    maskBit Nothing _ = [Zero, One]
    maskBit (Just One) _ = [One]
    maskBit (Just Zero) n = [n]

runInstrV2 :: State -> String -> State
runInstrV2 (St mem mask) s =
  case words s of
    "mask" : "=" : rest -> St mem (parseMask $ concat rest)
    ('m' : 'e' : 'm' : '[' : num) : "=" : rest ->
      let index = intToBits $ read $ take (length num - 1) num
          indices = applyMaskV2 mask index
          n = intToBits $ read $ concat $ rest
          mem' = foldr (flip M.insert n) mem indices
       in St mem' mask
    _ -> undefined

solveTwo :: String -> Integer
solveTwo =
  M.foldr ((+) . bitsToInt) 0
    . getMemory
    . foldl runInstrV2 (St M.empty (replicate 36 Nothing))
    . lines
