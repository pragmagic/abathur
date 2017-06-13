## Include file that contains the 'follow' proc which computes which
## branches of an inner BTree node need to be followed for an
## interval search.

type
  IntervalOption = enum
    minIsInf, maxIsInf,
    minExcluded, # <a
    maxExcluded, # <b
    maxIsMin # search for a single key
  Interval = object
    a, b: SepValue
    options: set[IntervalOption]
#[
binLowerBound(a, lo, hi, x)
  while lo <= hi:
    let mid = lo + (hi - lo) div 2
    if cmp(a[mid], key) >= 0:
      hi = mid - 1
    else
      lo = mid + 1
  return lo

binHigherBound(a, lo, hi, x)
  while lo <= hi:
    let mid = lo + (hi - lo) div 2
    if cmp(a[mid], key) > 0:
      hi = mid - 1
    else
      lo = mid + 1
  return lo
]#

proc rawSearchMin(key: SepValue; a: Node; b: BTree; start, last: int): int =
  ## we search the maximal v in a such that 'key <? v' (key is still
  ## less than or equal to 'v'). If exclusive  <?  is '<'
  ## This algorithm with different boundary cases is what 'insert' needs to use!
  var ri = last
  var le = start
  while le <= ri:
    let mid = le + (ri - le) div 2
    assert mid >= start and mid <= last
    if cmp(a[mid], key) >= 0:
      ri = mid - 1
    else:
      le = mid + 1
  result = le

proc searchMin(key: SepValue; a: Node; b: BTree; start, last: int;
               exclusive: bool): int =
  result = rawSearchMin(key, a, b, start, last)
  # boundary cases:
  if result > last: result = last
  elif result > 0 and cmp(a[result], key) != 0: dec result
  elif exclusive and result > 0 and cmp(a[result], key) == 0:
    # if the keys are identical and we require 'lt', we know
    # only the left branch is required:
    dec result

proc searchMaxHelper(key: SepValue; a: Node; b: BTree; start, last: int): int =
  # we want to find the last index 'i' where a[i] is still 'key'
  var ri = last
  var le = start
  result = start
  while le <= ri:
    let mid = le + (ri - le) div 2
    assert mid >= start and mid <= last
    if cmp(a[mid], key) == 0:
      # still the same key, so move 'le'
      le = mid + 1
      result = mid  # remember this
    else:
      ri = mid - 1

proc searchMax(key: SepValue; a: Node; b: BTree; start, last: int; exclusive: bool): int =
  var ri = last
  var le = start
  while le <= ri:
    let mid = le + (ri - le) div 2
    assert mid >= start and mid <= last
    if cmp(a[mid], key) < 0:
      le = mid + 1
    else:
      ri = mid - 1
  result = le
  # boundary cases:
  if result >= last:
    result = last
  else:
    # we need the last entry that could possibly be meant:
    result = searchMaxHelper(SepValue a[result], a, b, result, last)
  #elif result > 0 and cmp(a[result], key) == 0:
    # if the keys are identical and we require 'ge', we know
    # only the left branch is required:
  #  dec result
  if result > 0 and exclusive and cmp(a[result], key) == 0:
    dec result
  #elif result > 0 and not exclusive and cmp(a[result], key) < 0:
  #  dec result
  #echo "res ", result, "  ", start, "..", last

proc follow(wanted: Interval; keys: Node; b: BTree; last: int): (int, int) =
  if minIsInf notin wanted.options:
    result[0] = searchMin(wanted.a, keys, b, 1, last, minExcluded in wanted.options)
  else:
    result[0] = 0
  # due to possible duplicate keys, we cannot shortcut this search for max:
  #if maxIsMin in wanted.options:
  #  result[1] = result[0]
  if maxisInf notin wanted.options:
    result[1] = searchMax(wanted.b, keys, b, result[0], last,
                          maxExcluded in wanted.options)
    # it can be that keys[result[b]] > max and then we decrement it by one:
    if maxIsMin in wanted.options:
      if cmp(keys[result[1]], wanted.b) > 0: dec result[1]
  else:
    result[1] = last

proc binaryFind(key: SepValue; keys: Node; b: BTree; last: int): int =
  result = rawSearchMin(key, keys, b, 0, last)
  if result < 0 or result > last or cmp(keys[result], key) != 0:
    # not found:
    result = -1

proc matchMax(key: SepValue; a: Node; b: BTree; start, last: int; exclusive: bool): int =
  assert start >= 0
  var ri = last
  var le = start
  while le <= ri:
    let mid = le + (ri - le) div 2
    assert mid >= start and mid <= last
    if cmp(a[mid], key) < 0:
      le = mid + 1
    else:
      ri = mid - 1
  result = le
  # boundary cases:
  if result >= last:
    result = last
  else:
    # we need the last entry that could possibly be meant:
    result = searchMaxHelper(SepValue a[result], a, b, result, last)
  if exclusive:
    while result > start and cmp(a[result], key) == 0: dec result
  while result >= start and cmp(a[result], key) > 0: dec result

proc matches(wanted: Interval; keys: Node; b: BTree; last: int): (int, int) =
  assert cmp(wanted.a, wanted.b) != 0 or maxIsMin in wanted.options
  if maxIsMin in wanted.options:
    let x = binaryFind(wanted.a, keys, b, last)
    if x < 0: return (abs(x), -1)
    return (x, searchMaxHelper(SepValue keys[x], keys, b, x, last))

  if minIsInf notin wanted.options:
    result[0] = max(binaryFind(wanted.a, keys, b, last), 0)
    # could still be a non-match though:
    if minExcluded in wanted.options:
      let c = cmp(keys[result[0]], wanted.a)
      if c < 0: result[0] = last+1
      elif c == 0: inc result[0]
    else:
      if cmp(keys[result[0]], wanted.a) < 0: result[0] = last+1
    #searchMin(wanted.a, keys, 0, minExcluded in wanted.options)
  else:
    result[0] = 0
  if maxisInf notin wanted.options:
    result[1] = matchMax(wanted.b, keys, b, result[0], last,
                         maxExcluded in wanted.options) #binaryFind(keys, wanted.b, last)
    when false:
      if result[1] >= 0:
        result[1] = searchMaxHelper(keys[result[1]], keys, b, result[1], last)
        if maxExcluded in wanted.options:
          let c = cmp(wanted.b, keys[result[1]])
          if c < 0: result[1] = -2
          elif c == 0: dec result[1]
        else:
          if cmp(wanted.b, keys[result[1]]) < 0: result[1] = -2
      else:
        result[1] = abs(result[1])-2
    #result[1] = searchMax(wanted.b, keys, result[0],
    #                      maxExcluded in wanted.options)
  else:
    result[1] = last
