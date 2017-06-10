

type
  KV = object
    k, v: int
  F = object
    arrays: array[32, seq[KV]]

#proc insert(x: var F; k: KV) =

import random

proc randomStr(L: int): string =
  result = newString(L)
  for i in 0..<L:
    result[i] = chr(random(33..127))

#  X..Y and C..D overlap iff (X <= D and C <= Y)
#  X..<Y  and C..D overlap iff (X <= D and C < Y)
#  X..Y  and C..<D overlap iff (X < D and C <= Y)
# >X .. Y  and C..D overlap iff (D > X and C <= Y)
# >X .. Y  and C..<D overlap iff (D > X and C <= Y)

type
  Key = int

include follow

proc follow(wanted: Interval; keys: openArray[Key]): (int, int) =
  result = follow(wanted, keys, keys.high)

proc matches(wanted: Interval; keys: openArray[Key]): (int, int) =
  result = matches(wanted, keys, keys.high)

proc isEmpty(t: (int, int)): bool = t[0] > t[1]

proc main2 =
  block:
    #      unused  1   2   3   4   5   6    7    8
    let keys = [0, 0, 12, 14, 66, 88, 90, 100, 1000]
    doAssert follow(Interval(a: 0, b: 100, options: {maxExcluded}), keys) == (1, 6)
    doAssert follow(Interval(a: 0, b: 70, options: {}), keys) == (1, 5)

    doAssert follow(Interval(a: 0, b: 88, options: {maxExcluded}), keys) == (1, 4)

    doAssert follow(Interval(a: 12, b: 12, options: {maxIsMin}), keys) == (2, 2)

    doAssert follow(Interval(a: 13, b: 13, options: {maxIsMin}), keys) == (2, 2)

    doAssert follow(Interval(a: -12, b: 13, options: {}), keys) == (0, 3)

    doAssert follow(Interval(a: -12, b: 1000, options: {}), keys) == (0, 8)
    doAssert follow(Interval(a: -12, b: 1500, options: {}), keys) == (0, 8)

    doAssert follow(Interval(a: -12, b: 1000, options: {maxExcluded}), keys) == (0, 7)
    doAssert follow(Interval(a: 12, b: 13, options: {}), keys) == (2, 3)
    doAssert follow(Interval(a: 12, b: 13, options: {minExcluded}), keys) == (1, 3)
    doAssert follow(Interval(a: 12, b: -1, options: {maxIsInf}), keys) == (2, 8)
    doAssert follow(Interval(a: 1000, b: -1, options: {maxIsInf}), keys) == (8, 8)

    doAssert follow(Interval(a: 5000, b: 1000, options: {}), keys) == (8, 8)
    doAssert follow(Interval(a: -1, b: 89, options: {minIsInf}), keys) == (0, 6)

    doAssert follow(Interval(a: -1, b: 90, options: {minIsInf}), keys) == (0, 6)

    doAssert follow(Interval(a: -1, b: 90, options: {maxIsInf}), keys) == (0, 8)
    doAssert follow(Interval(a: 88, b: 90, options: {maxIsInf}), keys) == (5, 8)

  when true:
    echo "dup tests!"
    # now do the same with duplicate keys:
    #      unused  1   2   3   4   5   6   7   8
    let keys = [0, 0, 12, 12, 12, 12, 12, 12, 12]
    doAssert follow(Interval(a: 0, b: 100, options: {maxExcluded}), keys) == (1, 8)

    doAssert follow(Interval(a: 0, b: 12, options: {}), keys) == (1, 8)
    doAssert follow(Interval(a: 12, b: 12, options: {maxIsMin}), keys) == (2, 8)

    doAssert follow(Interval(a: -1, b: -1, options: {maxIsMin}), keys) == (0, 0)
    doAssert follow(Interval(a: 5000, b: 10000, options: {}), keys) == (8, 8)
    doAssert follow(Interval(a: 12, b: 20, options: {}), keys) == (2, 8)
    doAssert follow(Interval(a: 13, b: 20, options: {}), keys) == (8, 8)

    doAssert follow(Interval(a: 0, b: 90, options: {maxIsInf}), keys) == (1, 8)

  block:
    echo "matches tests!"
    #            0  1   2   3   4   5   6    7    8
    let keys = [-1, 0, 12, 14, 66, 88, 90, 100, 1000]
    doAssert matches(Interval(a: 12, b: 12, options: {maxIsMin}), keys) == (2, 2)
    doAssert matches(Interval(a: 0, b: 100, options: {}), keys) == (1, 7)

    doAssert matches(Interval(a: 0, b: 100, options: {maxExcluded}), keys) == (1, 6)
    doAssert matches(Interval(a: 0, b: 70, options: {}), keys) == (1, 4)

    doAssert matches(Interval(a: 12, b: 500, options: {}), keys) == (2, 7)
    doAssert matches(Interval(a: 12, b: 99, options: {}), keys) == (2, 6)
    doAssert matches(Interval(a: 12, b: 100, options: {}), keys) == (2, 7)
    doAssert matches(Interval(a: 12, b: 101, options: {}), keys) == (2, 7)
    doAssert matches(Interval(a: 12, b: 77, options: {}), keys) == (2, 4)

    doAssert matches(Interval(a: -1, b: 88, options: {maxExcluded}), keys) == (0, 4)

    doAssert(isEmpty matches(Interval(a: 13, b: 13, options: {maxIsMin}), keys))

    doAssert matches(Interval(a: -12, b: 13, options: {}), keys) == (0, 2)
    doAssert matches(Interval(a: -12, b: 88, options: {}), keys) == (0, 5)

    doAssert matches(Interval(a: -12, b: 1000, options: {}), keys) == (0, 8)
    doAssert matches(Interval(a: -12, b: 1500, options: {}), keys) == (0, 8)

    doAssert matches(Interval(a: -12, b: 1000, options: {maxExcluded}), keys) == (0, 7)

    doAssert matches(Interval(a: 12, b: 13, options: {}), keys) == (2, 2)
    #doAssert matches(Interval(a: 12, b: 13, options: {minExcluded}), keys) == (1, 2)
    doAssert matches(Interval(a: 12, b: -1, options: {maxIsInf}), keys) == (2, 8)
    doAssert matches(Interval(a: 1000, b: -1, options: {maxIsInf}), keys) == (8, 8)

    #doAssert matches(Interval(a: 5000, b: 1000, options: {}), keys) == (8, 8)
    doAssert matches(Interval(a: -1, b: 89, options: {minIsInf}), keys) == (0, 5)

    doAssert matches(Interval(a: -1, b: 90, options: {minIsInf}), keys) == (0, 6)

    doAssert matches(Interval(a: -1, b: 90, options: {maxIsInf}), keys) == (0, 8)
    doAssert matches(Interval(a: 88, b: 90, options: {}), keys) == (5, 6)
    doAssert(isEmpty matches(Interval(a: 1200, b: 0, options: {maxIsInf}), keys))
    doAssert(isEmpty matches(Interval(a: 1200, b: 2000, options: {}), keys))
    doAssert(isEmpty matches(Interval(a: -3200, b: -300, options: {minIsInf}), keys))
    doAssert(isEmpty matches(Interval(a: -3200, b: -300, options: {}), keys))

  block:
    #            0  1   2   3   4   5   6    7    8
    let keys = [-1, 0, 12, 12, 66, 66, 66, 100, 1000]
    doAssert matches(Interval(a: 12, b: 12, options: {maxIsMin}), keys) == (2, 3)
    doAssert matches(Interval(a: 0, b: 100, options: {}), keys) == (1, 7)

    doAssert matches(Interval(a: 0, b: 100, options: {maxExcluded}), keys) == (1, 6)
    doAssert matches(Interval(a: 0, b: 70, options: {}), keys) == (1, 6)

    doAssert matches(Interval(a: 12, b: 500, options: {}), keys) == (2, 7)
    doAssert matches(Interval(a: 12, b: 99, options: {}), keys) == (2, 6)
    doAssert matches(Interval(a: 12, b: 100, options: {}), keys) == (2, 7)
    doAssert matches(Interval(a: 12, b: 101, options: {}), keys) == (2, 7)
    doAssert matches(Interval(a: 12, b: 66, options: {}), keys) == (2, 6)

    doAssert matches(Interval(a: -1, b: 66, options: {maxExcluded}), keys) == (0, 3)
    doAssert(isEmpty matches(Interval(a: 13, b: 13, options: {maxIsMin}), keys))
    doAssert matches(Interval(a: -12, b: 13, options: {}), keys) == (0, 3)
    doAssert(isEmpty matches(Interval(a: -3200, b: -300, options: {minIsInf}), keys))
    doAssert(isEmpty matches(Interval(a: -3200, b: -300, options: {}), keys))
    doAssert(isEmpty matches(Interval(a: -3200, b: -300, options: {maxExcluded}), keys))

  echo "Yes."

proc main =
  for i in 0..1000:
    let x = randomStr(i+12)
    let y = substr(x, 0, 10)
    if not (y < x):
      echo "not smaller ", y
      echo x

main2()
