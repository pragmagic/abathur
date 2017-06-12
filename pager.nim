## Memory management for Abathur.

## All pointers that leave the BTree are *unowned* pointers,
## the BTree owns them. This is problematic if we want to
## unpage parts of the BTree. We use reference counting to
## ensure no page is unpaged that leaked pointers to the
## outside world (pin/unpin operations).

import memops, tables

const
  PageShift = 14
  PageSize = 1 shl PageShift # 16KB
  OffsetMask = PageSize-1
  OverflowPageSize = 1024 * 1024 * 2 # 2 MB

const
  isInternalFlag = 1 ## 0th bit for 'isInternal' node information
  isDirtyFlag = 2    ## 1th bit for 'isDirty'
  isHugeFlag = 4     ## 2th bit for 'isHuge'
  rcIncrement = 8
  rcShift = 3        ##

type
  PageId* = int64
  PageAddr = distinct int64 ## PageId * OverflowPageSize + offset
  NodeHeader {.inheritable, pure.} = object ## Node/Page header: 16 bytes
    id*: PageId
    m*: int32
    flags: int32  ## refcount and flags merged

  HugeNodeHeader = object of NodeHeader # 32 bytes
    size: int32
    padding: array[12, byte]

  Node* = ptr NodeHeader

const
  MaxKeys* = (PageSize - 16) div 2 # keys and values of size 1
# 16 = sizeof(NodeHeader)

template isInternal*(n: Node): bool = (n.flags and isInternalFlag) != 0
template setIsInternal*(n: Node) = n.flags = n.flags or isInternalFlag
template isDirty(n: Node): bool = (n.flags and isDirtyFlag) != 0
template isHuge(n: Node): bool = (n.flags and isHugeFlag) != 0

template getPageSize*(p: Node): int32 =
  if isHuge(p): cast[ptr HugeNodeHeader](p).size else: PageSize

type
  TypeKind* = enum ## serialized, so keep binary compatible
    tyNone, tyBool,
    tyByte, tyInt16, tyInt32, tyInt64, tyTime,
    tyFloat32, tyFloat64,
    tyUserFixed, ## an unknown type of fixed size
    tyString,
    tyTriple,    ## a triple of variadic types
    tyUserVar    ## an unknown type of variable size
                 ## (uses the same trick as 'tyString')
  TypeDesc* = object
    size*: byte
    kind*: TypeKind
    id*: int16  ## if not 0 used for type checking.
  AttrDesc* = object
    unique*: bool  # has the 'unique' restriction
    keyPos*: byte  # 0 -- not a key, otherwise N'th position of the lookup key

type
  PageMgr* = object
    maxPageId*: PageId
    totalMem: int ## how much memory we already use
    maxMem: int   ## how much memory we are allowed to use
    loaded: Table[PageId, Node]
    dirty: seq[Node]
    overflowPage: ptr HugeNodeHeader # \
      # for long data use a bump pointer allocator
      # into this substructure
    overflowIdx: int # bump pointer
    dir*: string
  Pager* = ptr PageMgr

  Comparator* = proc (a, b: pointer; p: Pager): int {.nimcall.}
  NodeLayout* = object
    cmp*: Comparator
    valsOffset*, linksOffset*: int
    leafPairs*: int # number of possible pairs for a leaf
    innerPairs*: int # number of possilbe pairs for a inner node
    keyDesc*, valDesc*: TypeDesc

  PinnedValue* = object
    p*: pointer
    n*: Node

  SepValue* = distinct pointer ## a value that is binary compatible
                               ## with how values are stored in the
                               ## BTree but separately allocated

proc countPairs(keySize, valSize: int): int =
  ## computes how many (key,val) pairs can be stored in the BTree node.
  var remaining = PageSize - sizeof(NodeHeader) - # Header takes room too
                             valSize        # for possible padding
                                            # between keys and vals arrays
  # for n pairs we require:
  result = remaining div (keySize+valSize)
  # n must be even:
  if (result and 1) == 1: dec result
  doAssert result > 2, "node must be able to store at least 4 pairs"

proc fillNodeLayout*(n: var NodeLayout; keyDesc, valDesc: TypeDesc) =
  n.leafPairs = countPairs(int keyDesc.size, int valDesc.size)
  n.valsOffset = roundup(sizeof(NodeHeader) + n.leafPairs * keyDesc.size.int,
                                              valDesc.size.int)
  n.innerPairs = countPairs(int keyDesc.size, sizeof(PageId))
  n.linksOffset = roundup(sizeof(NodeHeader) + n.innerPairs * keyDesc.size.int,
                                              valDesc.size.int)
  n.keyDesc = keyDesc
  n.valDesc = valDesc
  assert n.valsOffset mod valDesc.size.int == 0
  assert n.linksOffset mod sizeof(PageId) == 0
  assert n.valsOffset + (n.leafPairs * valDesc.size.int) <= PageSize
  assert n.linksOffset + (n.innerPairs * sizeof(PageId)) <= PageSize
  assert(sizeof(NodeHeader) + (n.keyDesc.size.int * n.leafPairs) <= n.valsOffset)
  assert(sizeof(NodeHeader) + (n.keyDesc.size.int * n.innerPairs) <= n.linksOffset)

template keyAt*(p: Node; i: int; n: NodeLayout): pointer =
  ## compute the address of the i'th key.
  p +! (sizeof(NodeHeader) + (n.keyDesc.size.int * i))

template valAt*(p: Node; i: int; n: NodeLayout): pointer =
  ## compute the address of the i'th value.
  p +! (n.valsOffset + (n.valDesc.size.int * i))

template linkAt*(p: Node; i: int; n: NodeLayout): pointer =
  ## compute the address of the i'th link.
  p +! (n.linksOffset + (sizeof(PageId) * i))

template pageIdAt*(p: Node; i: int; n: NodeLayout): PageId =
  ## compute the address of the i'th link.
  cast[ptr PageId](linkAt(p, i, n))[]

template storeLink*(p: Node; i: int; n: NodeLayout; link: PageId) =
  cast[ptr PageId](linkAt(p, i, n))[] = link

template pin*(p: Node) =
  ## keep 'p' in memory until 'unpin' is called
  inc p.flags, rcIncrement

template unpin*(p: Node) =
  dec p.flags, rcIncrement

## Values are stored in the same way as keys are so that
## values can be used as keys.
## Strings are Pascal 80ies style: A length byte followed by
## the data. Strings are declared as a fixed size though.
## If the length byte is 255 it is a "long string" and the
## payload is a PageId. This is aligned to the next 8 byte
## boundary though. It follows that a string is always at
## least 16 bytes as a key. This is acceptable.

proc initPageMgr*(pm: Pager; maxPageId: PageId; dir: string;
                  maxMem = 1024 * 1024 * 32) =
  pm.dir = dir
  pm.maxMem = maxMem
  pm.loaded = initTable[PageId, Node]()
  pm.dirty = @[]

proc getNewPageId(pm: Pager): PageId =
  # increment by 2 here so that PageIds are always odd.
  # This is used for fast pointer access in the BTree later.
  inc pm.maxPageId, 2
  result = pm.maxPageId

template toFilename(id): untyped = pm.dir & "/abathur_v1_" & $id & ".db"
const rootFilename* = "abathur_v1_root.db"

proc writeBack(pm: Pager; x: Node) =
  ## XXX distinguish between client/server here.
  x.flags = x.flags and not isDirtyFlag
  var f: File
  let dest = toFilename(x.id)
  var err = false
  let size = getPageSize(x)
  if open(f, dest, fmWrite):
    err = writeBuffer(f, x, size) != size
    close(f)
  else:
    err = true
  if err:
    #removeFile(dest) # a good idea? maybe if partially written?
    quit "unable to write file: " & dest

proc writeBack2(pm: Pager; id: PageId;
                h: pointer; hsize: int32;
                b: pointer; bsize: int32) =
  var f: File
  let dest = toFilename(id)
  var err = false
  if open(f, dest, fmWrite):
    err = writeBuffer(f, h, hsize) != hsize
    if not err:
      err = writeBuffer(f, b, bsize) != bsize
    close(f)
  else:
    err = true
  if err:
    #removeFile(dest) # a good idea? maybe if partially written?
    quit "unable to write file: " & dest

proc markDirty*(pm: Pager; p: Node) =
  if not p.isDirty:
    p.flags = p.flags or isDirtyFlag
    pm.dirty.add p

proc loadPage(pm: Pager; id: PageId): Node =
  var f: File
  let dest = toFilename(id)
  var err = false
  result = allocMem(Node, PageSize)
  pm.totalMem += PageSize
  # lazy is too late!!!!
  #pm.maxPageId = max(pm.maxPageId, id)
  if open(f, dest, fmRead):
    err = readBuffer(f, result, PageSize) != PageSize
    if not err and isHuge(result):
      let old = result
      let realSize = cast[ptr HugeNodeHeader](old).size.int
      result = allocMem(Node, realSize)
      storeMem(result, old, PageSize)
      deallocMem(old)
      let remaining = realSize - PageSize
      err = readBuffer(f, result +! PageSize, remaining) != remaining
      pm.totalMem += remaining
    close(f)
  else:
    err = true
  if err:
    when not defined(release): writeStackTrace()
    quit "unable to read file: " & dest

proc nuke(pm: Pager; p: Node) =
  assert p.flags shr rcShift == 0
  if isDirty(p):
    writeBack(pm, p)
    for i in 0..<pm.dirty.len:
      if pm.dirty[i] == p:
        del(pm.dirty, i)
        break
  pm.totalMem -= getPageSize(p)
  deallocMem(p)

proc victim(pm: Pager) =
  ## select a page to nuke to keep memory usage bounded.
  var firstChoice: Node = nil
  var secondChoice: Node = nil
  for candidate in pm.loaded.values:
    if candidate.flags shr rcShift == 0:
      if not isDirty(candidate):
        firstChoice = candidate
        break
      elif secondChoice == nil or isHuge(candidate):
        secondChoice = candidate

  if firstChoice != nil:
    pm.nuke(firstChoice)
  elif secondChoice != nil:
    pm.nuke(secondChoice)
  else:
    discard "live with the increased memory usage"

proc storeDirtyPages*(pm: Pager) =
  for i in 0..<pm.dirty.len:
    let p = pm.dirty[i]
    assert p.isDirty
    writeBack(pm, p)
  if pm.overflowPage != nil and isDirty(pm.overflowPage):
    writeBack(pm, pm.overflowPage)

proc at(p: PageAddr; pm: Pager): pointer =
  ## address translation.
  let id = PageId(p) div OverflowPageSize
  var x = if pm.overflowPage.id == id: pm.overflowPage
          else: getOrDefault(pm.loaded, id)
  if x == nil:
    # Since this is used by cmpStrings which accesses 2
    # pages, it's not safe to do this here:
    when false:
      if pm.totalMem >= pm.maxMem: victim(pm)
    x = pm.loadPage(id)
    pm.loaded[id] = x
  # We MUST NOT use 'or' here but '+%' because the pointer
  # might not be aligned on a page boundary.
  result = cast[pointer](cast[int](x) +% (PageId(p) mod OverflowPageSize))

## procs that start with 'take' must be followed by a 'drop'

proc pinNode*(pm: Pager; id: PageId): Node =
  ## You MUST call unpin afterwards!
  result = getOrDefault(pm.loaded, id)
  if result == nil:
    if pm.totalMem >= pm.maxMem:
      victim(pm)
    result = pm.loadPage(id)
    pm.loaded[id] = result
  pin result

proc rawFreshPage(pm: Pager; size: int32 = PageSize): Node =
  if pm.totalMem >= pm.maxMem: victim(pm)
  result = allocMem(Node, size)
  result.id = getNewPageId(pm)
  pm.totalMem += size

proc pinFreshNode*(pm: Pager): Node =
  ## You MUST call 'unpin' afterwards!
  result = rawFreshPage(pm, PageSize)
  pm.loaded[result.id] = result

proc freshHugePage(pm: Pager; size: int32): ptr HugeNodeHeader =
  # Do NOT add to dirty pages here!
  result = cast[ptr HugeNodeHeader](rawFreshPage(pm, size))
  result.flags = result.flags or isHugeFlag
  result.size = size

proc storeVla*(pm: Pager; p: pointer; size: int32): PageAddr =
  # does it still fit? If not, allocate something with enough room.
  # If longer than OverflowPageSize, don't allocate anything and
  # store it to a new file directly (zero copies made then).
  const align = 16
  assert sizeof(NodeHeader) == 16
  assert sizeof(HugeNodeHeader) == 32

  if size >= OverflowPageSize - sizeof(HugeNodeHeader):
    let h = HugeNodeHeader(id: getNewPageId(pm), m: 0, flags: isHugeFlag,
                           size: size+sizeof(HugeNodeHeader).int32)
    writeBack2(pm, h.id, unsafeAddr(h), sizeof(HugeNodeHeader).int32, p, size)
    return PageAddr(h.id * OverflowPageSize +% sizeof(HugeNodeHeader).int32)

  var target = roundup(pm.overflowIdx + size, align)
  if pm.overflowPage == nil or target >= pm.overflowPage.size:
    if pm.overflowPage != nil:
      pm.totalMem -= pm.overflowPage.size
      writeBack(pm, pm.overflowPage)
      deallocMem(pm.overflowPage)
    let s = roundup(max(OverflowPageSize, size+sizeof(HugeNodeHeader)), align)
    pm.overflowPage = freshHugePage(pm, s.int32)
    pm.overflowIdx = sizeof(HugeNodeHeader)
    assert pm.overflowIdx mod align == 0
    target = roundup(pm.overflowIdx + size, align)
  result = PageAddr(pm.overflowPage.id * OverflowPageSize +% pm.overflowIdx)
  storeMem(pm.overflowPage +! pm.overflowIdx, p, size)
  pm.overflowIdx = target

#[
  Only for documentation:

type
  LongString = object
    s: byte # always 255
    pad: array[3, byte]
    realLen: int32
    page: PageId  # if 0, use  thisPtr + 16
                  # as the address of the first byte instead
  ShortString = object
    s: byte # always < 255
    b: array[254, byte]
]#

const
  sizeOverflow* = 255
  minStringSize* = 16
  bestStringSize* = 63 # an educated guess
  stringAlignment* = 8

proc getAlignment*(t: TypeDesc): int =
  (if t.kind in {tyString, tyUserVar}: stringAlignment else: int t.size)

proc allocTempString*(p: pointer; size: int): SepValue =
  if size < sizeOverflow:
    result = allocMem(SepValue, size+1)
    let r = result.pointer
    storeByte(r, byte(size))
    storeMem(r +! 1, p, size)
  else:
    result = allocMem(SepValue, size+16)
    let r = result.pointer
    storeByte(r, sizeOverflow)
    storeInt32(r +! 4, size.int32)
    storeInt64(r +! 8, 0)
    storeMem(r +! 16, p, size)

proc allocInt64*(i: int64): SepValue =
  result = allocMem(SepValue, sizeof(int64))
  storeInt64(pointer result, i)

proc allocInt32*(i: int32): SepValue =
  result = allocMem(SepValue, sizeof(int32))
  storeInt32(pointer result, i)

proc allocTempString*(s: string): SepValue =
  result = allocTempString(unsafeAddr(s[0]), s.len)

proc deallocTempString*(p: SepValue) =
  deallocMem(pointer p)

template withTempString*(s: string; x, body: untyped) =
  let x = allocTempString(s)
  body
  deallocTempString(x)

template declareToSepValue(t) {.dirty.} =
  template toSepValue*(x: t): SepValue =
    var y = x
    SepValue(addr(y))

declareToSepValue(byte)
declareToSepValue(int16)
declareToSepValue(int32)
declareToSepValue(int64)
declareToSepValue(float32)
declareToSepValue(float64)

proc storeString(p: pointer; size: int; s: pointer; slen: int32;
                 pm: Pager) =
  assert size >= minStringSize and size <= (sizeOverflow+1)
  if slen < size:
    # store as a short string:
    storeByte(p, byte(slen))
    storeMem(p +! 1, s, slen)
  else:
    storeByte(p, sizeOverflow)
    storeInt32(p +! 4, slen)
    let z = storeVla(pm, s, slen)
    storeInt64(p +! 8, int64(z))

template extract(a, alen, adat) =
  alen = (type(alen)) fetchByte(a)
  if alen < sizeOverflow:
    # short string:
    adat = a +! 1
  else:
    alen = (type(alen)) fetchInt32(a +! 4)
    let page = fetchInt64(a +! 8)
    if page != 0:
      # no need for an RC op here, since the borrow is so short:
      adat = at(PageAddr(page), pm)
    else:
      adat = a +! 16

proc storeEntry*(dest: pointer; typ: TypeDesc; src: SepValue;
                 pm: Pager) =
  let src = pointer(src)
  case typ.kind
  of tyString, tyUserVar:
    var p: pointer
    var size: int32
    extract(src, size, p)
    storeString(dest, typ.size.int, p, size, pm)
  #of tyTriple:
  #  storeTriple(dest, src, pm)
  else:
    storeMem(dest, src, typ.size.int)

proc addEntry*(s: var string; p: pointer; typ: TypeDesc; pm: Pager) =
  case typ.kind
  of tyString, tyUserVar:
    var size = int(fetchByte(p))
    var q = p +! 1
    if size == sizeOverflow:
      size = fetchInt32(p +! 4)
      let z = fetchInt64(p +! 8)
      if z != 0:
        q = at(PageAddr(z), pm)
      else:
        q = p +! 16
    let L = s.len
    setLen(s, L+size)
    storeMem(addr(s[L]), q, size)
    when defined(debugNoBinary):
      for i in 0..<size:
        if cast[cstring](q)[i] < ' ':
          assert false, "invalid data!"
  of tyBool:
    let b = bool(fetchByte(p))
    s.add(if b: "true" else: "false")
  of tyByte:
    s.add int(fetchByte(p))
  of tyFloat32:
    s.add fetchFloat32(p)
  of tyFloat64:
    s.add fetchFloat64(p)
  of tyInt16:
    s.add int(fetchInt16(p))
  of tyInt32:
    s.add int(fetchInt32(p))
  of tyInt64:
    s.add fetchInt64(p)
  of tyUserFixed, tyTime, tyNone, tyTriple:
    discard "not implemented"

proc addSepValue*(s: var string; p: SepValue; typ: TypeDesc) =
  addEntry(s, pointer(p), typ, nil)

proc cmpStrings*(a, b: pointer; pm: Pager): int {.nimcall.} =
  var alen, blen: int
  var adat, bdat: pointer
  extract(a, alen, adat)
  extract(b, blen, bdat)
  result = cmpMem(adat, bdat, min(alen, blen))
  if result == 0:
    result = alen - blen

proc cmpTriples*(a, b: pointer; pm: Pager): int {.nimcall.} =
  discard

proc cmpInt64*(a, b: pointer; pm: Pager): int {.nimcall.} =
  # XXX unfortunately we can't use '-' here directly.
  let x = fetchInt64(a) - fetchInt64(b)
  if x < 0: result = -1
  elif x == 0: result = 0
  else: result = 1

proc cmpInt32*(a, b: pointer; pm: Pager): int {.nimcall.} =
  result = fetchInt32(a) - fetchInt32(b)

proc cmpInt16*(a, b: pointer; pm: Pager): int {.nimcall.} =
  result = fetchInt16(a) - fetchInt16(b)

proc cmpFloat64*(a, b: pointer; pm: Pager): int {.nimcall.} =
  # XXX unfortunately we can't use '-' here directly.
  let x = fetchFloat64(a) - fetchFloat64(b)
  if x < 0.0: result = -1
  elif x == 0.0: result = 0
  else: result = 1

proc cmpFloat32*(a, b: pointer; pm: Pager): int {.nimcall.} =
  # XXX unfortunately we can't use '-' here directly.
  let x = fetchFloat32(a) - fetchFloat32(b)
  if x < 0.0: result = -1
  elif x == 0.0: result = 0
  else: result = 1

proc cmpByte*(a, b: pointer; pm: Pager): int {.nimcall.} =
  result = fetchByte(a).int - fetchByte(b).int

proc typeToCmp*(t: TypeKind): Comparator =
  case t
  of tyBool, tyByte: result = cmpByte
  of tyInt16: result = cmpInt16
  of tyInt32: result = cmpInt32
  of tyInt64: result = cmpInt64
  of tyString: result = cmpStrings
  of tyTriple: result = cmpTriples
  else: result = nil

# Not yet implemented:
# Instead of a proper LRU cache, during BTree traversal we add candidates
# for elimination. This way, leaves are preferred and it seems smarter
# to use the 'mask' logic in the BTree instead of blind guessing based
# on access times.

when isMainModule:

  proc newString(data: pointer, size: int): string =
    result = newString(size)
    copyMem(addr(result[0]), data, size)

  var xs = ""
  for i in 0..100:
    xs.add "hey here thisfdlkasdfj klsafj salkfdj slkajf lksf jslkf sklf jsalfj ksdalf jsldfkj\n"

  withTempString(xs, y):
    var p: pointer
    var size: int32
    var pm: Pager = nil
    extract(pointer y, size, p)
    let ss = newString(p, size)
    echo ss == xs

  proc nodeLayout(keyDesc, valDesc: TypeDesc): NodeLayout =
    fillNodeLayout(result, keyDesc, valDesc)

  doAssert sizeof(NodeHeader) == 16
  doAssert sizeof(HugeNodeHeader) == 32
  let lo = nodeLayout(TypeDesc(kind: tyInt32, size: 4), TypeDesc(kind: tyString, size: 16))
  echo "4, 8: ", lo
  echo lo.valsOffset mod 8
