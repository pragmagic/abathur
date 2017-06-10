## BTree implementation for Karax DB. Can also be used as a persistent
## data structure. The persistent operations use a 'Ps' suffix.
## Can also use a page manager for allocations. The page manager can be used
## to off load pages to a file system or to send it over the wire.

## Todo:
## - Add logic to deal with the fact that keys do not have to be unique.

## Keys can be made unique by adding a 'count' part. This solution is easier
## to implement than lists of values. --> But this means every key is 4
## bytes larger than necessary. :-( It's better to keep keys small and
## instead make values larger.
## What is the problem though with multiple keys? The iterator can already
## follow potentially many branches to support range queries.
## Insertions work reasonably well with the existing algorithm. The major
## problem is that the key in the inner node is not discriminant anymore.
## We need to be able to mark such keys as "ambiguous" (both to the left
## and to the right keys of this value may occur). This is only a problem
## for an exact match of key==search_value. We could always make these
## matches ambiguous. This also implies to modify every binary search
## to deal with multiple keys of the same value. It has the advantage
## that no additional storage is required at all. -- Yeah, let's do
## that!

## Another possibility
## to detect ambiguous keys is to check the next key. If identical,
## it is not a discriminant key. This is slower though.

from strutils import startsWith
import memops, pager

## Due to the fact that leaves are shared among multiple BTrees the following
## fields in a Node are downright impossible:
## - parent
## - next
## - prev

type
  BTree* = object
    root: PageId
    height: int ## height
    n: int      ## total number of key-value pairs (select count(*) from x)
    layout*: NodeLayout
    pager: Pager
  CursorState = enum stPop, stLeaf, stEnd

proc newBTree*(root: PageId; keyDesc, valDesc: TypeDesc;
               cmp: Comparator;
               pager: Pager): BTree =
  result = BTree(root: root)
  fillNodeLayout(result.layout, keyDesc, valDesc)
  result.layout.cmp = cmp
  result.pager = pager

type
  IntervalOption = enum
    minIsInf, maxIsInf,
    minExcluded, # <a
    maxExcluded, # <b
    maxIsMin # search for a single key
  Interval = object
    a, b: SepValue
    options: set[IntervalOption]

template cmp(x, y: pointer): untyped =
  b.layout.cmp(x, y, b.pager)

template cmp(x: SepValue, y: pointer): untyped =
  b.layout.cmp(pointer x, y, b.pager)

template cmp(x, y: SepValue): untyped =
  b.layout.cmp(pointer x, pointer y, b.pager)

template myhigh(n: Node): int = MaxKeys+1
#  if n.isInternal: n.layout.innerPairs else: n.layout.leafPairs

proc searchMin(key: SepValue, n: Node; start: int; exclusive: bool;
               b: BTree): int =
  ## we search the maximal v in a such that 'key <? v' (key is still
  ## less than or equal to 'v'). If exclusive  <?  is '<'
  var ri = n.m.int
  var le = start
  while le < ri:
    var mid = (le + ri) div 2
    # leftmost keys are smaller than anything, rightmost are bigger than
    # anything:
    let c = if mid < start: -1
            elif mid >= n.m: 1
            else: cmp(key, keyAt(n, mid, b.layout))
    if c > 0: le = mid + 1
    elif c == 0:
      # exact match?  < a[mid]
      assert mid > 0
      # if the keys are identical and we require 'lt', we know
      # only the left branch is required:
      return mid-ord(exclusive)
    else: ri = mid
  result = le-1

proc searchMax(key: SepValue; n: Node; start: int; exclusive: bool;
               b: BTree): int =
  var ri = n.m.int
  var le = start
  while le < ri:
    var mid = (le + ri) div 2
    # leftmost keys are smaller than anything, rightmost are bigger than
    # anything:
    let c = if mid < start: -1
            elif mid >= n.m: 1
            else: cmp(key, keyAt(n, mid, b.layout))
    if c < 0: ri = mid
    elif c == 0:
      # exact match?  < a[mid]
      assert mid > 0
      # if the keys are identical and we require 'ge', we know
      # only the left branch is required:
      return min(mid, n.m-1)-ord(exclusive)
    else: le = mid + 1
  result = min(ri-1, n.m-1)

proc follow(wanted: Interval; keys: Node; b: BTree): (int, int) =
  if minIsInf notin wanted.options:
    result[0] = searchMin(wanted.a, keys, 1, minExcluded in wanted.options, b)
  else:
    result[0] = 0
  if maxIsMin in wanted.options:
    result[1] = result[0]
  elif maxisInf notin wanted.options:
    result[1] = searchMax(wanted.b, keys, result[0]+1,
                          maxExcluded in wanted.options, b)
  else:
    result[1] = keys.myhigh

proc binaryFind(v: Node, key: SepValue; b: BTree): int =
  var
    le = 0
    ri = v.m.int-1
  while le <= ri:
    let probe = (le + ri) div 2
    let c = cmp(key, keyAt(v, probe, b.layout))
    if c > 0:
      le = probe + 1
    elif c < 0:
      ri = probe - 1
    else:
      return probe
  result = -(le + 1)

proc matches(wanted: Interval; keys: Node; b: BTree): (int, int) =
  assert cmp(wanted.a, wanted.b) != 0 or maxIsMin in wanted.options
  if maxIsMin in wanted.options:
    let x = binaryFind(keys, wanted.a, b)
    if x < 0: return (abs(x), -1)
    return (x, x)

  if minIsInf notin wanted.options:
    result[0] = max(binaryFind(keys, wanted.a, b), 0)
    # could still be a non-match though:
    if minExcluded in wanted.options:
      let c = cmp(wanted.a, keyAt(keys, result[0], b.layout))
      if c > 0: result[0] = keys.m
      elif c == 0: inc result[0]
    else:
      if cmp(wanted.a, keyAt(keys, result[0], b.layout)) > 0: result[0] = high(int)
    #searchMin(wanted.a, keys, 0, minExcluded in wanted.options)
  else:
    result[0] = 0
  if maxisInf notin wanted.options:
    result[1] = binaryFind(keys, wanted.b, b)
    if result[1] >= 0:
      if maxExcluded in wanted.options:
        let c = cmp(wanted.b, keyAt(keys, result[1], b.layout))
        if c < 0: result[1] = -2
        elif c == 0: dec result[1]
      else:
        if cmp(wanted.b, keyAt(keys, result[1], b.layout)) < 0: result[1] = -2
    else:
      result[1] = abs(result[1])-2
    #result[1] = searchMax(wanted.b, keys, result[0],
    #                      maxExcluded in wanted.options)
  else:
    result[1] = keys.myhigh

const
  wordShift = 5
  wordMask = uint32((1 shl wordShift) - 1)

type
  QueryOp* = enum
    opTrue, opCmp, opIn, opNot, opAnd, opOr, opStartsWith

  BTreeQuery* = ref object ## a query against a single BTree, keep in mind
                           ## this only covers a single index...
    case op*: QueryOp
    of opTrue: discard
    of opCmp, opStartsWith:
      i: Interval
    of opIn:
      t: BTree
      innerq: BTreeQuery
      keys: bool
    of opNot:
      arg0: BTreeQuery
    of opAnd, opOr:
      arg1, arg2: BTreeQuery

  Mask = array[(MaxKeys shr wordShift) + 1, uint32]
  QCursor* = object
    state: CursorState
    stack: seq[PageId]
    m: Mask
    i: int
    n: Node

proc setBit(m: var Mask; i: int) {.inline.} =
  m[i shr wordShift] = m[i shr wordShift] or (1u32 shl (uint32(i) and wordMask))

proc testBit(m: Mask; i: int): bool {.inline.} =
  (m[i shr wordShift] and (1u32 shl (uint32(i) and wordMask))) != 0u32

proc initQCursor*(x: PageId): QCursor =
  result.stack = @[x]
  result.i = 0
  result.n = nil
  result.state = stPop

proc initQCursor*(t: BTree): QCursor = initQCursor(t.root)

proc atEnd*(c: QCursor): bool = c.state == stEnd

proc getKey*(c: QCursor; b: BTree): pointer =
  assert c.state == stLeaf
  result = keyAt(c.n, c.i, b.layout)

proc getPinnedKey*(c: QCursor; b: BTree): PinnedValue =
  assert c.state == stLeaf
  result.p = keyAt(c.n, c.i, b.layout)
  result.n = c.n
  pin(c.n)

proc getVal*(c: QCursor; b: BTree): pointer =
  assert c.state == stLeaf
  result = valAt(c.n, c.i, b.layout)

proc getPinnedVal*(c: QCursor; b: BTree): PinnedValue =
  assert c.state == stLeaf
  result.p = valAt(c.n, c.i, b.layout)
  result.n = c.n
  pin(c.n)

proc next*(c: var QCursor; b: BTree; q: BTreeQuery) {.gcsafe.}

iterator elements(q: BTreeQuery): pointer =
  var c = initQCursor(q.t.root)
  while true:
    next(c, q.t, q.innerq)
    if atEnd(c): break
    yield if q.keys: getKey(c, q.t) else: getVal(c, q.t)

proc evalAtom(n: Node; q: BTreeQuery; m: var Mask; b: BTree) =
  assert(not n.isInternal)
  case q.op
  of opTrue:
    for i in 0..<n.m: setBit(m, i)
  of opCmp:
    let (a, b) = matches(q.i, n, b)
    for i in a..b: setBit(m, i)
  of opStartsWith:
    # XXX Implement me
    # XXX this can be sped up even further with a binary search:
    #for i in 0..<n.m:
    #  if n.keys[i].startsWith(q.i.a): setBit(m, i)
    discard
  of opIn:
    for against in elements(q):
      let (a, b) = matches(Interval(a: SepValue(against), options: {maxIsMin}), n, b)
      if a <= b: setBit(m, a)
  of opNot:
    evalAtom(n, q.arg0, m, b)
    for i in 0 .. n.m shr wordShift: m[i] = not m[i]
  of opAnd:
    var mm: Mask
    evalAtom(n, q.arg1, mm, b)
    evalAtom(n, q.arg2, m, b)
    for i in 0 .. n.m shr wordShift: m[i] = m[i] and mm[i]
  of opOr:
    # we simply write into the same 'm' mask:
    evalAtom(n, q.arg1, m, b)
    evalAtom(n, q.arg2, m, b)

proc evalInner(n: Node; q: BTreeQuery; m: var Mask; b: BTree) =
  assert n.isInternal
  case q.op
  of opCmp, opStartsWith:
    let (a, b) = follow(q.i, n, b)
    for i in a..b: setBit(m, i)
  of opIn:
    var card = 0
    for against in elements(q):
      # 'in' is based on equality:
      let (a, bb) = follow(Interval(a: SepValue(against), options: {maxIsMin}), n, b)
      if a <= bb and not testBit(m, a):
        setBit(m, a)
        inc card
        # we already follow every branch, no need to further
        # evaluate the set...
        if card == b.layout.innerPairs: return
  of opNot, opTrue:
    # consider:  not (k < 3)
    # The fact that for k < 3 we only have to visit the 0th child doesn't
    # imply that for 'not (k < 3)' we only have to visit all other chilren!
    # Instead we have to follow everything. The query parser seeks to eliminate
    # 'not' for this reason.
    for i in 0 .. n.m shr wordShift: m[i] = high(uint32)
  of opAnd:
    var mm: Mask
    evalInner(n, q.arg1, mm, b)
    evalInner(n, q.arg2, m, b)
    for i in 0 .. n.m shr wordShift: m[i] = m[i] and mm[i]
  of opOr:
    # we simply write into the same 'm' mask:
    evalInner(n, q.arg1, m, b)
    evalInner(n, q.arg2, m, b)

proc next(c: var QCursor; b: BTree; q: BTreeQuery) =
  while true:
    case c.state
    of stEnd: return
    of stLeaf:
      let x = c.n
      for j in c.i+1 ..< x.m:
        if testBit(c.m, j):
          # state stays stLeaf
          c.i = j
          return
      c.state = stPop
    of stPop:
      if c.stack.len == 0:
        c.state = stEnd
        return
      let x = pinNode(b.pager, c.stack.pop())
      zMem(addr(c.m), sizeof(c.m))
      if not x.isInternal:
        c.i = -1
        c.n = x
        c.state = stLeaf
        evalAtom(x, q, c.m, b)
      else:
        evalInner(x, q, c.m, b)
        for i in countdown(x.m-1, 0):
          if testBit(c.m, i): c.stack.add(pageIdAt(x, i, b.layout))
        # state stays stPop, but go on.
      unpin(x)

#proc get(t: BTree; key: Key): Val = search(t.root, key, t.height)

proc copyHalf(h, result: Node; mhalf: int; b: BTree) =
  storeMem(keyAt(result, 0, b.layout), keyAt(h, mhalf, b.layout),
           b.layout.keyDesc.size.int * mhalf)
  #for j in 0 ..< Mhalf:
  #  result.keys[j] = h.keys[mhalf + j]
  assert result.isInternal == h.isInternal
  if h.isInternal:
    storeMem(linkAt(result, 0, b.layout), linkAt(h, mhalf, b.layout),
             sizeof(PageId) * mhalf)
    #for j in 0 ..< Mhalf:
    #  result.links[j] = h.links[offset + j]
  else:
    storeMem(valAt(result, 0, b.layout), valAt(h, mhalf, b.layout),
             b.layout.valDesc.size.int * mhalf)
    #for j in 0 ..< Mhalf:
    #  shallowCopy(result.vals[j], h.vals[offset + j])

proc split(h: Node; b: BTree): PageId =
  ## split node in half
  let mhalf = if h.isInternal: (b.layout.innerPairs shr 1)
              else: (b.layout.leafPairs shr 1)
  let res = pinFreshNode(b.pager)
  res.m = mhalf.int32
  if h.isInternal: setIsInternal(res)
  h.m = mhalf.int32
  copyHalf(h, res, mhalf, b)
  # markDirty(b.pager, h)
  # insert below calls 'markDirty' for 'h'
  markDirty(b.pager, res)
  result = res.id
  unpin res

proc insert(p: PageId, key, val: SepValue; b: BTree): PageId =
  let h = pinNode(b.pager, p)
  var newLink: PageId = 0
  var j = 0
  var M = 0
  if not h.isInternal:
    while j < h.m:
      if cmp(key, keyAt(h, j, b.layout)) < 0: break
      inc j
    for i in countdown(h.m.int, j+1):
      storeMem(valAt(h, i, b.layout), valAt(h, i-1, b.layout), b.layout.valDesc.size.int)
      # shallowCopy(h.vals[i], h.vals[i-1])
    storeEntry(valAt(h, j, b.layout), b.layout.valDesc, val, b.pager)
    M = b.layout.leafPairs
  else:
    while j < h.m:
      if j+1 == h.m or cmp(key, keyAt(h, j+1, b.layout)) < 0:
        let u = insert(pageIdAt(h, j, b.layout), key, val, b)
        inc j
        if u == 0:
          unpin(h)
          return 0
        newLink = u
        break
      inc j
    for i in countdown(h.m.int, j+1):
      storeMem(linkAt(h, i, b.layout), linkAt(h, i-1, b.layout), sizeof(PageId))
    storeLink(h, j, b.layout, newLink)
    M = b.layout.innerPairs

  for i in countdown(h.m.int, j+1):
    storeMem(keyAt(h, i, b.layout), keyAt(h, i-1, b.layout), b.layout.keyDesc.size.int)
  if newLink != 0:
    let u = pinNode(b.pager, newLink)
    storeMem(keyAt(h, j, b.layout), keyAt(u, 0, b.layout), b.layout.keyDesc.size.int)
    unpin u
  else:
    storeEntry(keyAt(h, j, b.layout), b.layout.keyDesc, key, b.pager)
  inc h.m
  result = if h.m < M: PageId(0) else: split(h, b)
  markDirty(b.pager, h)
  unpin(h)

when false:
  # XXX not yet ported
  proc insertPs(h: Node, key: Key, val: Val): (Node, Node) =
    var j = 0
    var hh = Node(m: h.m, isInternal: h.isInternal)
    for i in 0 ..< h.m: hh.keys[i] = h.keys[i]
    if not h.isInternal:
      while j < h.m:
        if less(key, h.keys[j]): break
        inc j
      for i in countdown(h.m, j+1): shallowCopy(hh.vals[i], h.vals[i-1])
      for i in 0 ..< j: shallowCopy(hh.vals[i], h.vals[i])
      for i in countdown(h.m, j+1): hh.keys[i] = hh.keys[i-1]
      hh.vals[j] = val
      hh.keys[j] = key
    else:
      # could optimize this copying here:
      for i in 0 ..< h.m: hh.links[i] = h.links[i]
      while j < h.m:
        if j+1 == h.m or less(key, h.keys[j+1]):
          let (root, r) = insertPs(h.links[j], key, val, ht-1)
          hh.links[j] = root
          if r == nil:
            return (hh, nil)
          else:
            inc j
            for i in countdown(h.m, j+1):
              hh.links[i] = hh.links[i-1]
              hh.keys[i] = hh.keys[i-1]
            hh.links[j] = r
            hh.keys[j] = r.keys[0]
            break
        inc j
    inc hh.m
    return if hh.m < M: (hh, nil) else: (hh, split(hh))

proc put*(b: var BTree; key, val: SepValue) =
  let u = insert(b.root, key, val, b)
  inc b.n
  if u == 0: return

  # need to split root
  let t = pinFreshNode(b.pager)
  t.m = 2
  setIsInternal(t)
  let root = pinNode(b.pager, b.root)
  let uu = pinNode(b.pager, u)
  #t.keys[0] = b.root.keys[0]
  storeMem(keyAt(t, 0, b.layout), keyAt(root, 0, b.layout), b.layout.keyDesc.size.int)
  #t.links[0] = b.root
  storeLink(t, 0, b.layout, b.root)
  #t.keys[1] = u.keys[0]
  storeMem(keyAt(t, 1, b.layout), keyAt(uu, 0, b.layout), b.layout.keyDesc.size.int)
  #t.links[1] = u
  storeLink(t, 1, b.layout, u)

  b.root = t.id
  inc b.height
  markDirty(b.pager, t)
  unpin t
  unpin root
  unpin uu

when false:
  # XXX not yet ported
  proc putPs*(b: BTree; key: Key; val: Val): BTree =
    let (root, u) = insertPs(b.root, key, val, b.height)
    result.n = b.n + 1
    result.height = b.height
    if u == nil:
      result.root = root
      return
    # need to split root
    let t = Node(m: 2, isInternal: true)
    t.keys[0] = root.keys[0]
    t.links[0] = root
    t.keys[1] = u.keys[0]
    t.links[1] = u
    result.root = t
    inc result.height

proc newBTreeQueryEq*(key: SepValue): BTreeQuery =
  BTreeQuery(op: opCmp, i: Interval(a: key, b: key, options: {maxIsMin}))

proc toString(id: PageId, indent: string; result: var string; b: BTree) =
  let h = pinNode(b.pager, id)
  if not h.isInternal:
    for j in 0..<h.m:
      result.add(indent)
      result.addEntry(keyAt(h, j, b.layout), b.layout.keyDesc, b.pager)
      result.add " "
      result.addEntry(valAt(h, j, b.layout), b.layout.valDesc, b.pager)
      result.add "\n"
  else:
    for j in 0..<h.m:
      if j > 0:
        result.add(indent & "(")
        result.addEntry(keyAt(h, j, b.layout), b.layout.keyDesc, b.pager)
        result.add ")\n"
      toString(pageIdAt(h, j, b.layout), indent & "  ", result, b)
  unpin(h)

proc `$`*(b: BTree): string =
  result = ""
  toString(b.root, "", result, b)

proc put*(t: var BTree; key, val: int32) =
  t.put(toSepValue(key), toSepValue(val))

proc put*(t: var BTree; key, val: string) =
  withTempString(key, k):
    withTempString(val, v):
      t.put(k, v)

proc put*(t: var BTree; key: string; val: int32) =
  withTempString(key, k):
    t.put(k, toSepValue(val))

when isMainModule:
  var mgr: PageMgr
  initPageMgr(addr mgr, 1, ".")

  proc main2 =
    let desc = TypeDesc(kind: tyInt32, size: 4)
    let x = pinFreshNode(addr mgr)
    var t = newBTree(x.id, desc, desc, cmpInt32, addr mgr)
    unpin(x)
    for i in 1i32..50000i32:
      t.put(i, 50i32 - i)
    echo t

  proc main3 =
    let desc = TypeDesc(kind: tyString, size: 16)
    let x = pinFreshNode(addr mgr)
    var t = newBTree(x.id, desc, desc, cmpStrings, addr mgr)
    unpin(x)
    for i in 1i32..50000i32:
      let k = $i & "adflongerthan16charsherepleasebugtriggering"
      t.put(k, $(50i32 - i))
    echo t
    echo t.height, " ", t.n

  proc main =
    when false:
      var st = newBTree()
      st.put("www.cs.princeton.edu", "abc")
      st.put("www.cs.princeton.edu", "xyz")
      st.put("www.princeton.edu",    "128.112.128.15")
      st.put("www.yale.edu",         "130.132.143.21")
      st.put("www.simpsons.com",     "209.052.165.60")
      st.put("www.apple.com",        "17.112.152.32")
      st.put("www.amazon.com",       "207.171.182.16")
      st.put("www.ebay.com",         "66.135.192.87")
      st.put("www.cnn.com",          "64.236.16.20")
      st.put("www.google.com",       "216.239.41.99")
      st.put("www.nytimes.com",      "199.239.136.200")
      st.put("www.microsoft.com",    "207.126.99.140")
      st.put("www.dell.com",         "143.166.224.230")
      st.put("www.slashdot.org",     "66.35.250.151")
      st.put("www.espn.com",         "199.181.135.201")
      st.put("www.weather.com",      "63.111.66.11")
      st.put("www.yahoo.com",        "216.109.118.65")

      assert st.get("www.cs.princeton.edu") == "abc"
      assert st.get("www.harvardsucks.com") == nil

      assert st.get("www.simpsons.com") == "209.052.165.60"
      assert st.get("www.apple.com") == "17.112.152.32"
      assert st.get("www.ebay.com") == "66.135.192.87"
      assert st.get("www.dell.com") == "143.166.224.230"
      assert(st.n == 17)
      echo st

    when false:
      var bd = newBTree()
      for i in 1..50:
        bd.put("a", "b")
        bd.put("z", "b")
      echo bd
      echo bd.n, " height ", bd.height

    when false:
      var b2 = newBTree()
      const iters = 10_000
      for i in 1..iters:
        b2.put($i, $(iters - i))
      for i in 1..iters:
        let x = b2.get($i)
        if x != $(iters - i):
          echo "got ", x, ", but expected ", iters - i
      echo b2.n
      echo b2.height

    when false:
      var b1 = newBTree()
      var b2 = newBTree()
      const iters = 9 #60_000
      for i in 1..iters:
        b2 = b2.putPs($i, $(iters - i))
        b1.put($i, $(iters - i))
      for i in 1..iters:
        let x = b2.get($i)
        if x != $(iters - i):
          echo i, "th iteration; got ", x, ", but expected ", iters - i
      echo b2.n, " = ", b1.n
      echo b2.height, " = ", b1.height
      echo " >= 5"
      dos(b1.root, CmpKind.ge, "5", proc(k: Key; v: Val) = echo("k ", k, " = ", v))
      echo " <= 5"
      dos(b1.root, CmpKind.le, "5", proc(k: Key; v: Val) = echo("k ", k, " = ", v))

      echo " == 5"
      dos(b1.root, CmpKind.eq, "5", proc(k: Key; v: Val) = echo("k ", k, " = ", v))
      echo " < 5"
      dos(b1.root, CmpKind.lt, "5", proc(k: Key; v: Val) = echo("k ", k, " = ", v))
      echo " > 5"
      dos(b1.root, CmpKind.gt, "5", proc(k: Key; v: Val) = echo("k ", k, " = ", v))

      echo "======================================================================"
      echo " >= 5"
      don(b1.root, CmpKind.ge, "5", proc(k: Key; v: Val) = echo("k ", k, " = ", v))
      echo " <= 5"
      don(b1.root, CmpKind.le, "5", proc(k: Key; v: Val) = echo("k ", k, " = ", v))

      echo " == 5"
      don(b1.root, CmpKind.eq, "5", proc(k: Key; v: Val) = echo("k ", k, " = ", v))
      echo " < 5"
      don(b1.root, CmpKind.lt, "5", proc(k: Key; v: Val) = echo("k ", k, " = ", v))
      echo " > 5"
      don(b1.root, CmpKind.gt, "5", proc(k: Key; v: Val) = echo("k ", k, " = ", v))

      echo "======================================================================"
      var c = initCursor(b1.root)
      var i = 0
      while true:
        next(c, CmpKind.le, "9")
        if atEnd(c): break
        echo "key ", getKey(c), " ", getVal(c)
        if i > 30: break
        inc i

      echo "======================================================================"
      var cq = initQCursor(b1.root)
      let q = BTreeQuery(op: opCmp, i: Interval(a: "4", b: "", options: {maxIsInf, minExcluded}))
      i = 0
      while true:
        next(cq, q)
        if atEnd(cq): break
        echo "keyo ", getKey(cq), " ", getVal(cq)
        if i > 30: break
        inc i

  main3()
