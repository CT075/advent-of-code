import Data.Maybe
import qualified Data.Set as S

type Deck = [Int]

member = S.member

playerOne =
  [ 26,
    16,
    33,
    8,
    5,
    46,
    12,
    47,
    39,
    27,
    50,
    10,
    34,
    20,
    23,
    11,
    43,
    14,
    18,
    1,
    48,
    28,
    31,
    38,
    41
  ]

playerTwo =
  [ 45,
    7,
    9,
    4,
    15,
    19,
    49,
    3,
    36,
    25,
    24,
    2,
    21,
    37,
    35,
    44,
    29,
    13,
    32,
    22,
    17,
    30,
    42,
    40,
    6
  ]

playRound :: Deck -> Deck -> Maybe (Deck, Deck)
playRound (x : xs) (y : ys) =
  if x > y
    then Just (xs ++ [x, y], ys)
    else Just (xs, ys ++ [y, x])
playRound _ _ = Nothing

playGame :: Deck -> Deck -> Deck
playGame xs ys =
  case playRound xs ys of
    Just (xs', ys') -> playGame xs' ys'
    Nothing -> xs ++ ys

solveOne :: () -> Int
solveOne () =
  sum $ zipWith (*) [1 ..] $ reverse $ playGame playerOne playerTwo

takeExact :: Int -> [a] -> Maybe [a]
takeExact 0 _ = Just []
takeExact _ [] = Nothing
takeExact n (x : xs) = (x :) <$> takeExact (n -1) xs

recursiveCombat :: S.Set (Deck, Deck) -> Deck -> Deck -> Either Deck Deck
recursiveCombat _ p1 [] = Left p1
recursiveCombat _ [] p2 = Right p2
recursiveCombat seen p1@(x : xs) p2@(y : ys) =
  if (p1, p2) `member` seen
    then Left p1
    else
      let seen' = S.insert (p1, p2) seen
       in case (takeExact x xs, takeExact y ys) of
            (_, Nothing) -> uncurry (recursiveCombat seen') $ fromJust $ playRound p1 p2
            (Nothing, _) -> uncurry (recursiveCombat seen') $ fromJust $ playRound p1 p2
            (Just xs', Just ys') ->
              case recursiveCombat seen' xs' ys' of
                Left _ -> recursiveCombat seen' (xs ++ [x, y]) ys
                Right _ -> recursiveCombat seen' xs (ys ++ [y, x])

unEither :: Either a a -> a
unEither (Left x) = x
unEither (Right x) = x

solveTwo :: () -> Int
solveTwo () =
  sum $
    zipWith (*) [1 ..] $
      reverse $ unEither $ recursiveCombat S.empty playerOne playerTwo
