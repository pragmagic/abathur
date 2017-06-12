
# include file

type
  SymKind* {.pure.} = enum
    Meta, Table, Index, Attribute

  AttrInfo* = object
    kind*: SymKind
    btree*: int32 # which btree this belongs to
    offset*: int32  # offset into key or value space to retrieve this attribute
    typ*: TypeDesc
    ad*: AttrDesc

  RelationInfo* = object # overlaid in memory with 'AttrInfo'
    kind*: SymKind
    idx*: int32
    valSize*: int32 # we know its kind is tyUserFixed
    keyDesc*: TypeDesc

  MetaInfo* = object # overlaid in memory with 'AttrInfo'
    kind*: SymKind
    pageId*: int64

  BiggestInfoType = AttrInfo # currently 'AttrInfo' is the biggest object,
                             # so we use that
template infoSize(): untyped = sizeof(BiggestInfoType)

proc initSchema(db: var Db; pm: Pager) =
  let y = pinFreshNode(pm)
  let strTy = TypeDesc(kind: tyString, size: 16)
  let valTy = TypeDesc(kind: tyUserFixed, size: infoSize())
  db.schema = newBTree(y.id, strTy, valTy, cmpStrings, pm)
  unpin(y)
  db.relations = @[]

proc createRelation(db: var Db; pm: Pager; ri: RelationInfo) =
  let y = pinFreshNode(pm)
  let valTy = TypeDesc(kind: tyUserFixed, size: ri.valSize)
  setLen(db.relations, ri.idx + 1)
  db.relations[ri.idx] = newBTree(y.id, ri.keyDesc, valTy, typeToCmp(ri.keyDesc.kind), pm)
  unpin(y)

proc putTableName(db: var Db; name: SepValue; ri: RelationInfo) =
  var b: BiggestInfoType
  # sizeof(ri) is correct here, we "widen" the type to the biggest type
  storeMem(addr(b), unsafeAddr(ri), sizeof(ri))
  db.schema.put(name, SepValue(addr(b)))

proc declareTable(db: var Db; pm: Pager; n: QStmt) =
  assert infoSize() >= sizeof(AttrInfo)
  assert infoSize() >= sizeof(RelationInfo)
  assert infoSize() >= sizeof(MetaInfo)

  assert n.kind == nkTable
  assert n[0].kind == nkLit
  var keyOffset = 0
  var valOffset = 0
  let btreeIdx = len(db.relations)

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
    let attrName = it[0].lit
    var aa: AttrInfo
    aa.btree = btreeIdx
    aa.kind = Attribute
    aa.typ = it.typ
    aa.ad = it.ad
    let align = getAlignment(it.typ)
    if it.ad.keyPos == 0:
      incOffset(valOffset)
    else:
      incOffset(keyOffset)
      if ri.keyDesc.kind == tyNone: ri.keyDesc = aa.typ
      else: doAssert false, "combined keys are not yet supported"
    db.schema.put(it[0].lit, SepValue(addr(aa)))

  ri.kind = Table
  ri.idx = btreeIdx
  ri.valSize = int32(valOffset)
  putTableName(db, n[0].lit, ri)
  createRelation(db, pm, ri)
