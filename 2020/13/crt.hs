-- solved part 1 in the python REPL

-- Copied from stack overflow
crt :: [(Integer, Integer)] -> (Integer, Integer)
crt = foldr go (0, 1)
  where
    go (r1, m1) (r2, m2) = (r `mod` m, m)
      where
        r = r2 + m2 * (r1 - r2) * (m2 `inv` m1)
        m = m2 * m1

    -- Modular Inverse
    a `inv` m = let (_, i, _) = gcd a m in i `mod` m

    -- Extended Euclidean Algorithm
    gcd 0 b = (b, 0, 1)
    gcd a b = (g, t - (b `div` a) * s, s)
      where
        (g, s, t) = gcd (b `mod` a) a

mkPairs :: String -> [(Integer, Integer)]
mkPairs s =
  let s' = map delComment s
      nums = zip [0 ..] $ words s'
   in nums >>= mkPair
  where
    delComment ',' = ' '
    delComment x = x

    mkPair :: (Integer, String) -> [(Integer, Integer)]
    mkPair (_, "x") = []
    mkPair (i, n) = [(- i, read n)]

solve2 :: String -> Integer
solve2 = fst . crt . mkPairs
