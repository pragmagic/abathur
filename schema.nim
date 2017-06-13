# included from vm.nim

type
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


# We map the schema to a single relation to keep memory usage low.
# However, this relation is hard to work with and speed critical to
# access for the query engine and so we also have a completely
# different representation in memory.
# The memory specific parts start here:
type
  RelationRef* = ref object
    info*: RelationInfo
    attrs*: seq[AttrInfo]

proc getTable*(db: var Db; name: string): RelationInfo =
  discard

proc getAttr*(db: var Db; pos: int32; ri: RelationInfo): AttrInfo =
  var cur = initTCursor(db.schema)
  while true:
    next(cur, db.schema)
    if atEnd(cur): break
    let ai = cast[ptr AttrInfo](getVal(cur, db.schema))
    if ai.kind == SymKind.Attribute and ai.pos == pos and
        ai.btree == ri.idx:
      result = ai[]
      break

proc initSchema*(db: var Db) =
  let y = pinFreshNode(db.pm)
  let strTy = TypeDesc(kind: tyString, size: 16)
  let valTy = TypeDesc(kind: tyUserFixed, size: infoSize())
  db.schema = newBTree(y.id, strTy, valTy, cmpStrings, db.pm)
  unpin(y)
  db.relations = @[]

proc createRelation(db: var Db; ri: RelationInfo) =
  let y = pinFreshNode(db.pm)
  let valTy = TypeDesc(kind: tyUserFixed, size: ri.valSize)
  setLen(db.relations, ri.idx + 1)
  db.relations[ri.idx] = newBTree(y.id, ri.keyDesc, valTy,
                                  typeToCmp(ri.keyDesc.kind), db.pm)
  unpin(y)

proc putTableName(db: var Db; name: SepValue; ri: RelationInfo) =
  var b: BiggestInfoType
  # sizeof(ri) is correct here, we "widen" the type to the biggest type
  storeMem(addr(b), unsafeAddr(ri), sizeof(ri))
  db.schema.put(name, SepValue(addr(b)))

proc declareTable(db: var Db; n: QStmt) =
  assert infoSize() >= sizeof(AttrInfo)
  assert infoSize() >= sizeof(RelationInfo)
  assert infoSize() >= sizeof(MetaInfo)

  assert n.kind == nkTable
  assert n[0].kind == nkLit
  var keyOffset = 0i32
  var valOffset = 0i32
  let btreeIdx = len(db.relations).int32

  template incOffset(off) =
    inc off, align - (off mod align)
    aa.offset = off
    inc off, aa.typ.size

  var ri: RelationInfo
  for i in 0..<n.len:
    let it = n[i]
    assert it.kind == nkAttrDef
    assert it.len == 1
    assert it[0].kind == nkLit
    let attrName = it[0].v
    var aa: AttrInfo
    aa.btree = btreeIdx
    aa.kind = SymKind.Attribute
    aa.typ = it.typ
    aa.ad = it.attr
    aa.pos = i.int32
    let align = getAlignment(aa.typ)
    if aa.ad.keyPos == 0:
      incOffset(valOffset)
    else:
      incOffset(keyOffset)
      if ri.keyDesc.kind == tyNone: ri.keyDesc = aa.typ
      else: doAssert false, "combined keys are not yet supported"
    db.schema.put(it[0].v, SepValue(addr(aa)))

  ri.kind = SymKind.Table
  ri.idx = btreeIdx
  ri.valSize = valOffset
  putTableName(db, n[0].v, ri)
  createRelation(db, ri)
