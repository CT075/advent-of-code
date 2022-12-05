
readtable =: 3 : 0
  d =. 1!:1 y
  d =. toJ d
  d =. cutopen d
  d =. 0 ". each d
  d =. > d
)

pre =: 25 " 0

NB. this isn't quite correct, because it includes 0, but I already know that 0
NB. isn't an input, so whatever.
ck_expanded =: 3 : 0
  last =. _1 {. y
  avail =. _1 }. y
  len =. # avail
  NB. matrix of 1s with 0s along the diagonal
  wanted =. -. (i. len) =/ (i. len)
  NB. (avail +/ avail) is all pairwise sums of avail
  sums =. wanted * avail +/ avail

  NB. (a e. b) = a `mem` b
  NB. (~.) = nub
  NB. (;) = flatten
  last e. ~. ; sums
)

NB. Don't panic!
NB.
NB. (_1 & {.) = last
NB. (f e. g) x = (f x) e. (g x)
NB. (f @: g) x = f (g x)
NB. (_1 & }.) = all but last
NB. (+/ ]) x = x +/ ] x = x +/ x = all pairwise sums
NB. (i. @: #) = range 0 to len-1
NB. (i.@:# =/ i.@:#) = 0 matrix with 1s along diagonal
NB. (-.) = boolean negate
NB. (-. @: (i.@:# =/ i.@:#)) = construct 1s matrix with 0s along diagonal of
NB.   size len(input) by len(input)
NB. (f * g) x = (f x) * (g x)
NB. (+/ ])*(thing two above) = matrix of sums excluding diagonal
NB. ~.@:;@:(above) = flatten and dedup all sums
ck =: (_1&{.)e.((~.@:;@:((+/ ])*-.@:(i.@:# =/ i.@:#)))@:(_1&}.))

NB. (|:) = transpose
NB. (1+pre 0)&(-.@:ck)\ = apply [ck] and negate to every continguous (1+pre 0)
NB.   sub-array
NB. # = "select" -- 0 1 0 # 1 2 3 = 2
NB. ((pre 0) & }.) = drop (pre 0)
solve_one =: (|:@:((1+pre 0)&((-.@:ck)\)))#((pre 0)&}.)

NB. This solution doesn't actually work, because for some godforsaken reason
NB. it insists that (bad = sums) is a length error, regardless of how much I
NB. insist that (bad) is a scalar. However, pasting and inlining the lines and
NB. results into the REPL gives the right answer, so whatever.
solve_two_expanded =: 3 : 0
  data =. readtable < y
  bad =. solve_one data
  NB. once all elements are greater than [bad], we don't care anymore
  data =. (|. +./\ |. bad >/ data) # data
  len =. $ data
  prefix_sums =. +/ \ data
  sums =. prefix_sums -/ prefix_sums
  loc =. (bad = sums) * (i. len, len)
  row =. <. loc % len
  col =. loc |~ len
  span =. (row - col) {. (col+1) }. data
  (>./ span) + (<./ span)
)

