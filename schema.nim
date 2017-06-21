# included from vm.nim

type
  Db* = object
    pm*: Pager
    schema*: BTree # attribute descriptors; strings -> AttrAddress
                   # string "" is special and contains in 'btree'
                   # the number of BTrees and in 'offset' the actual
                   # page number
    relations*: seq[BTree]

  SymKind* {.pure.} = enum
    Meta, Table, Index, Attribute

  AttrInfo* = object
    kind*: SymKind
    btree*: int32 # which btree this belongs to
    offset*: int32  # offset into key or value space to retrieve this attribute
    typ*: TypeDesc
    ad*: AttrDesc
    pos*: int32

  RelationInfo* = object # overlaid in memory with 'AttrInfo'
    kind*: SymKind
    idx*: int32
    valSize*: int32 # we know its kind is tyUserFixed
    keyDesc*: TypeDesc

  MetaInfo* = object # overlaid in memory with 'AttrInfo'
    kind*: SymKind
    nextPageId*: int64

  BiggestInfoType = AttrInfo # currently 'AttrInfo' is the biggest object,
                             # so we use that
template infoSize(): untyped = sizeof(BiggestInfoType).int32

template getByName(typ, cond) {.dirty.} =
  var cur = initRCursor(db.schema)
  while true:
    next(cur, db.schema, Interval(a: name, b: name, options: {maxIsMin}))
    if atEnd(cur): break
    let it = cast[ptr typ](getVal(cur, db.schema))
    if cond:
      result = it[]
      break

template isEmpty(x: AttrInfo|RelationInfo|MetaInfo): bool =
  x.kind == SymKind.Meta

proc getTable*(db: var Db; name: SepValue): RelationInfo =
  getByName(RelationInfo, it.kind == SymKind.Table)

proc getAttr*(db: var Db; name: SepValue; ri: RelationInfo): AttrInfo =
  getByName(AttrInfo, it.kind == SymKind.Attribute and it.btree == ri.idx)

proc getAttr*(db: var Db; pos: int32; ri: RelationInfo): (SepValue, AttrInfo) =
  var cur = initTCursor(db.schema)
  while true:
    next(cur, db.schema)
    if atEnd(cur): break
    let ai = cast[ptr AttrInfo](getVal(cur, db.schema))
    if ai.kind == SymKind.Attribute and ai.pos == pos and
        ai.btree == ri.idx:
      result[0] = SepValue getKey(cur, db.schema)
      result[1] = ai[]
      break

proc initSchema*(db: var Db) =
  let strTy = TypeDesc(kind: tyString, size: 16)
  let valTy = TypeDesc(kind: tyUserFixed, size: infoSize())
  let lay = initNodeLayout(strTy, valTy)
  let y = pinFreshNode(db.pm, false, lay)
  db.schema = newBTree(y.id, lay, cmpStrings, db.pm)
  unpin(y)
  db.relations = @[]

proc createRelation(db: var Db; ri: RelationInfo) =
  let valTy = TypeDesc(kind: tyUserFixed, size: ri.valSize)
  let lay = initNodeLayout(ri.keyDesc, valTy)
  let y = pinFreshNode(db.pm, false, lay)
  setLen(db.relations, ri.idx + 1)
  db.relations[ri.idx] = newBTree(y.id,lay,
                                  typeToCmp(ri.keyDesc.kind), db.pm)
  unpin(y)

proc putTableName(db: var Db; name: SepValue; ri: RelationInfo) =
  var b: BiggestInfoType
  # sizeof(ri) is correct here, we "widen" the type to the biggest type
  storeMem(addr(b), unsafeAddr(ri), sizeof(ri))
  db.schema.put(name, SepValue(addr(b)))

