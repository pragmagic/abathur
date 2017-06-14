
import
  compiler / [ast, astalgo, idents, msgs, parser, renderer] # we use Nim's parser here for now.

import vm, pager

proc startsWith(n: PNode; s: string): bool =
  result = n.safeLen >= 1 and n.kind in nkCallKinds and n[0].kind == nkIdent and
    n[0].ident.s == s

proc error(i: TLineInfo; msg: string) =
  echo i, " Error: ", msg

const
  skTable = skMethod
  skIndex = skIterator
  skAttr = skProc

type
  SymTab = ref object
    t: TStrTable
    types: TStrTable
    vars: int
    cache: IdentCache

{.experimental.}
using
  c: SymTab
  n: PNode

proc handleVar(c; n): VarId =
  var x = strTableGet(c.t, n.ident)
  if x == nil:
    x = newSym(skVar, n.ident, nil, n.info)
    x.position = c.vars
    #echo "var ", n.ident.s, " got ID ", x.position
    inc c.vars
    c.t.strTableAdd(x)
  if x.position > high(VarId):
    error n.info, "too many variables, rethink your query"
    result = VarId(0)
  else:
    result = VarId(x.position)

proc selDecl*(c; n): QStmt =
  result = tree(nkYield)
  for i in 1..<n.len:
    var it = n[i]
    if it.startsWith("?"):
      it = it[1]
    if it.kind == nkIdent:
      result.add ?handleVar(c, it)
    else:
      error it.info, "identifier expected"

proc texpr(c; n): QStmt

proc tcmp(k: QStmtKind; c; n): QStmt =
  if n.len == 3:
    result = tree(k, texpr(c, n[1]), texpr(c, n[2]))
  else:
    error n.info, "binary relation takes two arguments"

proc texpr(c; n): QStmt =
  case n.kind
  of nkIdent:
    # should be a predicate, but we check this later in the VM:
    result = name(n.ident.s)
  of nkStrLit..nkTripleStrLit:
    result = lit(n.strVal)
  of nkIntLit..nkInt64Lit:
    result = lit(n.intVal)
  of nkDotExpr:
    result = tree(nkDot, texpr(c, n[0]), texpr(c, n[1]))
  of nkCallKinds:
    if n[0].kind == nkIdent:
      case n[0].ident.s
      of "?":
        if n.len == 2 and n[1].kind == nkIdent:
          result = ?handleVar(c, n[1])
        else:
          error n.info, "illformed variable"
      of "==": result = tcmp(nkEq, c, n)
      of "!=": result = tcmp(nkNeq, c, n)
      of "<": result = tcmp(nkLt, c, n)
      of "<=": result = tcmp(nkLe, c, n)
      of ">": result = tcmp(nkGt, c, n)
      of ">=": result = tcmp(nkGe, c, n)
      of "and": result = tcmp(nkAnd, c, n)
      of "or": result = tcmp(nkOr, c, n)
      of "not": result = tcmp(nkNot, c, n)
      elif n.len == 3:
        result = tree(nkPred, texpr(c, n[0]), texpr(c, n[1]), texpr(c, n[2]))
      else:
        error n.info, "invalid expression: " & $n
    else:
      error n.info, "invalid expression: " & $n
  else:
    error n.info, "invalid expression: " & $n
  if result == nil:
    result = tree(vm.nkEmpty)

proc tselect(c; s, w: PNode): QStmt =
  result = tree(nkSelect)
  result.add selDecl(c, s)
  var cond = tree(nkAnd)
  if w.len != 2:
    error w.info, "where clause takes a single condition"
  elif w[1].kind == nkStmtList:
    for it in w[1]:
      cond.add texpr(c, it)
  else:
    cond.add texpr(c, w[1])
  result.add cond

proc tinsert(c; n): QStmt =
  if n.kind in nkCallKinds and n.len >= 2 and n[0].kind == nkIdent:
    # XXX support for named arguments here
    result = tree(nkInsert, name(n[0].ident.s))
    for i in 1..<n.len: result.add(texpr(c, n[i]))
  else:
    error n.info, "illformed 'insert' command"

proc toIdent(n): string =
  case n.kind
  of nkIdent: shallowCopy(result, n.ident.s)
  of nkStrLit..nkTripleStrLit: shallowCopy(result, n.strVal)
  else:
    error n.info, "identifier expected, but found " & $n
    result = ""

proc ttype(c; n): (TypeDesc, AttrDesc) =
  assert n.kind == nkStmtList
  if n.len == 1:
    var t = n[0]
    var a: AttrDesc
    var typ: TypeDesc
    if t.startsWith"unique_key" and t.len == 2:
      a.unique = true
      a.keyPos = 1
      t = t[1]
    elif t.startsWith"key" and t.len == 2:
      a.keyPos = 1
      t = t[1]
    if t.startsWith"string" and t.len == 2 and t[1].kind == nkIntLit:
      typ.kind = pager.tyString
      var size = t[1].intVal
      if size < minStringSize: size = minStringSize
      elif size >= sizeOverflow: size = sizeOverflow-1
      typ.size = size.int32
    elif t.kind == nkIdent:
      let x = strTableGet(c.types, t.ident)
      if x == nil: error n.info, "unknown type name: " & $n
      else:
        typ.kind = pager.TypeKind(x.position)
        typ.size = x.offset.int32
    else:
      error n.info, "invalid type: " & $n
    result = (typ, a)
  else:
    error n.info, "illformed type: " & $n

proc ttable(c; n): QStmt =
  result = tree(nkTable)
  if n.len == 3 and n[1].kind == nkIdent and n[2].kind == nkStmtList:
    # add table name:
    result.add name(toIdent(n[1]))
    let fields = n[2]
    for f in fields:
      if f.kind in nkCallKinds and f.len == 2 and f[1].kind == nkStmtList:
        let attr = tree(nkAttrDef)
        attr.setAttrProps ttype(c, f[1])
        attr.add name(toIdent(f[0]))
        result.add attr
      else:
        error f.info, "': type' expected"
  else:
    error n.info, "illformed 'table' command"

proc tindex(c; n): QStmt =
  if n.len == 3:
    discard
  else:
    error n.info, "illformed 'index' command"

proc parse*(c; s: string; filename = ""; line = 0): QStmt =
  let n = parseString(s, c.cache, filename, line)
  result = tree(nkQueryList)
  if n.kind == nkStmtList and n.len >= 1:
    var i = 0
    while i < n.len:
      let it = n[i]
      if i < n.len - 1 and it.startsWith"select" and n[i+1].startsWith("where"):
        result.add tselect(c, it, n[i+1])
        inc i
      elif it.startsWith"insert" and it.len == 2:
        result.add tinsert(c, it[1])
        # XXX 'insert' needs to allow inserting from a select statement!
      elif it.startsWith"table":
        result.add ttable(c, it)
      elif it.startsWith"index":
        result.add tindex(c, it)
      elif it.kind in {TNodeKind.nkEmpty, nkCommentStmt}:
        discard
      else:
        error it.info, "expected a statement"
        break
      inc i
  else:
    error n.info, "expected a statement"

template declareType(name: string; k: pager.TypeKind; size: int) =
  let s = newSym(skType, result.cache.getIdent(name), nil, unknownLineInfo())
  s.position = int(k)
  s.offset = size
  strTableAdd(result.types, s)

proc createPredicateMap*(): SymTab =
  new result
  initStrTable(result.t)
  initStrTable(result.types)
  result.vars = 0
  result.cache = newIdentCache()
  declareType("string", pager.tyString, bestStringSize)
  declareType("int", pager.tyInt32, 4)
  declareType("int16", pager.tyInt16, 2)
  declareType("int32", pager.tyInt32, 4)
  declareType("int64", pager.tyInt64, 8)
  declareType("time", pager.tyTime, 8)
  declareType("float32", pager.tyFloat32, 4)
  declareType("float64", pager.tyFloat64, 8)
  declareType("byte", pager.tyByte, 1)
  declareType("bool", pager.tyBool, 1)

when isMainModule:
  var m = createPredicateMap()
  discard parse(m, """
table person: # no 'key/value' annotations: first entry is the key
  id: key(int64)  # uniqueKey(int64)
  name: string(60)
  salary: int32
  tax: int32

table attribute:
  name: unique_key(string(31))
  btree: int32


# every attribute has a type --> BTree from attribute names to type
# every attribute has a way to access things:
# attribute name -> (BTree-Index, key|value, offset)
# table name -> BTree-Index

# We store the type multiple times to save a BTree.
# This attribute description allows us to update potential indexes
# automatically.

index person_name(name):
  id
""")
