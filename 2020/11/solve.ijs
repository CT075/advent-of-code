readlayout =: 3 : 0
  d =. 1!:1 y
  d =. toJ d
  d =. cutopen d
  > d
)

slice_spec =. 1 1 ,: 3 3

step_individual =: 3 : 0
  curr =. (< 1;1) { y
  fl =. curr = '.'
  occ =. curr = '#'
  nums =. > ('#' & =) each > y
  NB. (+/ & ;) = sum . flatten
  NB. ((< 1;1) & {) = item at index 1,1
  NB. ((f g h) x) = (f x) g (h x)
  NB.
  NB. so this line sums the box and subtracts the middle value, hence getting
  NB.   the number of adjacent, filled chairs.
  adj =. ((+/ & ;) - ((< 1;1) & {)) nums
  if. fl do. '.' else.
    if. occ do.
      if. (adj >: 4) do. 'L' else. curr end.
    else.
      if. (adj = 0) do. '#' else. curr end.
    end.
  end.
)

one =: 3 : 0
  curr =. (< 1;1) { y
  fl =. curr = '.'
  occ =. curr = '#'
  adj =. ((+/&;)-((<1;1)&{))>('#'&=)each>y
  etf =. (]&'L')`(]&'#')@.(=&0)
  fte =. (]&'L')`(]&'#')@.(<&4)
  ((etf`fte@.(]&occ))`(]&'.')@.(]&fl))adj
)

step =: 3 : 0
  padded =. '.',"1('.',y,'.'),.'.'
  NB. step_individual =. (+/ & ;) - ((< 1;1) & {)
  slice_spec step_individual ;. _3 padded
)

part_one =: 3 : 0
  seats =. readlayout < y
  final =. step ^: _ seats
  counts =. > ('#' & =) each > final
  +/ ; counts
)

NB. angles =: (< 1 0) , (< 0 1) , (< 1 1) , (< 1 _1)
NB. angles =: angles , <"1 (_1 _1 *"1 > angles)
NB. the verb applied to angles above is "multiply each coordinate by -1, -1"
angles =: (, (<"1 @: (_1 _1 *"1 >))) (<1 0),(<0 1),(<1 1),(<1 _1)

NB. From here, I wasn't able to figure out how to bend J to my will enough to
NB. accomplish what I wanted to do.
NB.
NB. The struggle came in building line-of-sight vectors for each seat. My
NB. initial plan was to do it by constructing a table of each seat coordinate,
NB. then using [angles] above to draw rays out in all 8 directions, but that
NB. became pretty painful pretty quickly and I wasn't particularly having fun
NB. anymore.

