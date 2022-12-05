
readtable =: 3 : 0
  d =. 1!:1 y
  d =. toJ d
  d =. cutopen d
  d =. 0 ". each d
  d =. > d
)

NB. to derive the below from above, just substitute the values of [d] in-place,
NB. then remove all parentheses.
readtable_tacit =: 3 : '>0 ". each cutopen toJ 1!:1 y'

NB. second argument minus first argument
NB. {: = all but last element
NB. {. = all but first element
NB. ({: - {.) t = ({: t) - ({. t)
diff =: {: - {.

part_one =: 3 : 0
  NB. y is always the input to a monadic (single-argument) function
  data =. readtable y
  sorted =. /:~ data
  NB. (x f \ y) means "apply f to successive runs of length x from y"
  NB. in this case, it means "apply diff to adjacent pairs (2) in y"
  diffs =. /:~ (2 diff \ sorted)
  NB. (= t) is "self-classify", which is equivalent to ((nub t) =/ t)
  NB. (nub t) is the standard deduplication function
  NB. ((nub t) =/ t) compares each element of (nub t) to each element of t,
  NB.   giving 1 when they match and 0 when they don't
  NB. This gives a table with 2 rows and n columns
  separated =. = diffs
  NB. (/) is effectively foldr -- it interleaves the elements of the table
  NB.   with the given operator. So (+/ 1 2 3 4) is (1+2+3+4).
  NB. (+/) " 1 says "apply the verb (+/) at rank 1 (per row)".
  counts =. +/ " 1 separated
  NB. Increment the number of 1-increases for the first connection (0-1), and
  NB.   also the number of 3-increases for the final connection (last-device).
  NB. This isn't strictly correct if the lowest adapter has voltage 3, but I
  NB.   already know that it has voltage 1. Properly, we should just prepend a
  NB.   0 voltage to [sorted] before doing all this, then add only to the 3s
  NB.   column.
  amended =. 1 + counts
  answer =. */ amended
  answer
)

part_one_tacit =: 3 : '*/ 1 &+ +/ " 1 = 2 ({: - {.) \ /:~ /:~ readtable < y'

trib =: 3 : '{. ((1 & }. , +/) ^: y) 1 1 2'

part_two =: 3 : 0
  data =. readtable y
  sorted =. /:~ data
  NB. remember to add the initial plug
  sorted =. 0, sorted
  diffs =. 2 diff \ sorted
  NB. remember to add the final output
  diffs =. diffs, 3
  NB. Okay, deep breath:
  NB.   ({.) = head
  NB.   (1 & }.) = drop 1
  NB.   (+/) = sum
  NB.   ((1 & }.) , +/) t = ((1 & }.) t) , (+/ t)
  NB.   (^: y) = repeat y times
  NB.   1 1 2 = first three elements of the tribonacci sequence
  trib =. 3 : '{. ((1 & }. , +/) ^: y) 1 1 2'
  NB. for each element in [diffs], replace it with a [1] if it is equal to 1,
  NB.   otherwise replace with 0. Effectively maps all 3s to 0s
  values =. 1= / diffs
  NB. see above. replace all 3s with 1s and 1s with 0s
  partitions =. 3= / diffs
  sums =. partitions +/ ;. 2 values
  combinations =. trib each sums
  answer =. */ > combinations
  answer
)

NB. I give up on inlining the [trib] call. The best I got was [dyad_trib],
NB.   which works like so:
NB.
NB.   4 dyad_trib 1 1 2 ==> 7 13 24
NB.
NB.   However, the obvious next step to make it monadic, bonding it to the
NB.   initial values [1 1 2], doesn't seem to work, and I can't find any
NB.   literature on how to accomplish this.
dyad_trib =: (1 & }.) , +/ ^:

part_two_tacit =: 3 : '*/ > trib each ((3 & (=/)) +/ ;. 2 (1 & (=/))) (, & 3) 2 ({: - {.) \ (0 & ,) /:~ readtable < y'

