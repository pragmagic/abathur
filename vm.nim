
import btree, pager

const
  MaxVars = 50

type
  VarId* = range[0..MaxVars-1]

  Value = SepValue
  QStmtKind* = enum
    nkEmpty,
    nkLit,
    nkRelation, # only for queries, not for the VM
    nkProc, # can do things like 'x + 1' or something else with a single value
    nkVarDef, # variable definition
    nkVarUse, # variable usage
    nkCall,   # call an nkProc(args)
    nkAsgn,   # assign to variable
    nkValues,
    nkKeys,
    nkPairs,
    nkFor,
    nkYield,  # also an alias for 'select'
    nkIf,
    nkInsert,
    nkCreateTable,
    nkSelect, # a 'select' statement is a pair of (nkYield,nkAnd(cond list))
    nkPred,   # a triple like '?subj pred ?obj'; pred comes first though
    nkLt, nkLe, nkEq, nkNeq, nkGt, nkGe,
    nkAnd, nkOr, nkNot, nkBetween

type
  QStmt* = ref object
    case kind*: QStmtKind
    of nkEmpty: discard
    of nkLit:
      v: Value
    of nkRelation: rid: int
    of nkProc: fn: proc (x: openArray[Value]): Value {.nimcall.}
    of nkVarDef, nkVarUse:
      varId: VarId
    of nkValues, nkKeys, nkPairs:
      tree: BTree
      cond: BTreeQuery
    else: kids: seq[QStmt]
    typ: TypeDesc
    compare: Comparator
  Action = proc (results: array[VarId, PinnedValue]; types: array[VarId, TypeDesc])
  Plan = ref object
    action: Action
    pager: Pager
    names: array[VarId, string] # kept for debugging
    bindings: array[VarId, PinnedValue]
    vartypes: array[VarId, TypeDesc]

proc `[]`*(n: Qstmt; i: int): QStmt {.inline.} = n.kids[i]
proc len*(n: QStmt): int {.inline.} = n.kids.len
proc add*(n, kid: QStmt) {.inline.} = n.kids.add(kid)
iterator items*(n: QStmt): QStmt =
  for i in 0..<n.len: yield n[i]
proc `lastSon=`*(n, kid: QStmt) = n.kids[^1] = kid

proc toString(n: QStmt, indent: string; result: var string) =
  result.add indent
  result.add($n.kind)
  result.add " "
  case n.kind
  of nkEmpty: discard
  of nkLit: result.addSepValue n.v, n.typ
  of nkRelation: result.add "rel " & $n.rid
  of nkProc: result.add "<some proc>"
  of nkVarDef, nkVarUse: result.add $n.varId
  of nkValues, nkKeys, nkPairs:
    result.add "<some iterator>"
  else:
    for x in n:
      toString(x, indent & "  ", result)

proc `$`*(n: QStmt): string =
  result = ""
  toString(n, "\n", result)

# we have a stmt/expr split here:
proc evalVal(p: Plan; it: QStmt): Value =
  case it.kind
  of nkLit: result = it.v
  of nkVarUse: result = Value(p.bindings[it.varId].p)
  of nkCall:
    var args = newSeq[Value](it.len-1)
    assert it[0].kind == nkProc
    for i in 0..it.len-2:
      args[i] = evalVal(p, it[i+1])
    result = it[0].fn(args)
  else: doAssert false, "invalid value evaluation"

template withRegion(p: Plan; body: untyped) =
  ## XXX to implement
  body

template cmp(a, b: SepValue): int = it.compare(pointer(a), pointer(b), p.pager)

template isTrue(v: SepValue): bool = pointer(v) != nil

proc evalBool(p: Plan; it: QStmt): bool =
  withRegion p:
    case it.kind
    of nkLt: result = cmp(evalVal(p, it[0]), evalVal(p, it[1])) < 0
    of nkLe: result = cmp(evalVal(p, it[0]), evalVal(p, it[1])) <= 0
    of nkEq: result = cmp(evalVal(p, it[0]), evalVal(p, it[1])) == 0
    of nkNeq: result = cmp(evalVal(p, it[0]), evalVal(p, it[1])) != 0
    of nkGt: result = cmp(evalVal(p, it[0]), evalVal(p, it[1])) > 0
    of nkGe: result = cmp(evalVal(p, it[0]), evalVal(p, it[1])) >= 0
    of nkAnd:
      result = true
      for i in 0..<it.len:
        if not evalBool(p, it[i]):
          result = false
          break
    of nkOr:
      result = false
      for i in 0..<it.len:
        if evalBool(p, it[i]):
          result = true
          break
    of nkNot: result = not evalBool(p, it[0])
    of nkBetween:
      let x = evalVal(p, it[0])
      result = cmp(evalVal(p, it[1]), x) <= 0 and cmp(x, evalVal(p, it[2])) <= 0
    of nkCall:
      result = isTrue evalVal(p, it)
    else: doAssert false, "invalid cond evaluation"

proc bindVar(p: Plan; varId: VarId; value: PinnedValue) =
  if p.bindings[varId].n != nil: unpin(p.bindings[varId].n)
  p.bindings[varId] = value

proc unpinVars(p: Plan) =
  for x in p.bindings:
    let n = x.n
    if n != nil: unpin(n)

proc exec(p: Plan; it: QStmt) =
  ## we can also translate a query plan at compile-time to a Nim program.
  case it.kind
  of nkFor:
    let iter = it[^2]
    let body = it[^1]
    var c = initQCursor(iter.tree)
    case iter.kind
    of nkValues:
      let cond = newBTreeQueryEq(SepValue p.bindings[it[0].varId].p)
      let valId = it[1].varId
      while true:
        next(c, iter.tree, cond)
        if atEnd(c): break
        bindVar p, valId, getPinnedVal(c, iter.tree)
        exec(p, body)
    of nkKeys:
      let keyId = it[0].varId
      let wanted = p.bindings[it[0].varId].p
      while true:
        next(c, iter.tree, iter.cond)
        if atEnd(c): break
        if it.compare(getVal(c, iter.tree), wanted, p.pager) == 0:
          bindVar p, keyId, getPinnedKey(c, iter.tree)
          exec(p, body)
    of nkPairs:
      let keyId = it[0].varId
      let valId = it[1].varId
      while true:
        next(c, iter.tree, iter.cond)
        if atEnd(c): break
        bindVar p, keyId, getPinnedKey(c, iter.tree)
        bindVar p, valId, getPinnedVal(c, iter.tree)
        exec(p, body)
    else:
      doAssert(false, "invalid for loop iterator")
  of nkYield:
    p.action(p.bindings, p.vartypes)
    unpinVars(p)
  of nkIf:
    let cond = it[0]
    let body = it[1]
    if evalBool(p, cond): exec(p, body)
  of nkAsgn:
    assert it[0].kind == nkVarDef
    let varId = it[0].varId
    if p.bindings[varId].n != nil:
      unpin(p.bindings[varId].n)
      p.bindings[varId].n = nil
    # XXX this will leak memory
    p.bindings[varId].p = pointer evalVal(p, it[1])
  else:
    doAssert false, "invalid control flow"

# ------------ query building and analysing --------------------------------

type
  Query = QStmt # we're lazy and use the same internal representation for both

proc tree*(k: QStmtKind; kids: varargs[QStmt]): QStmt =
  new result
  result.kind = k
  result.kids = newSeq[QStmt](kids.len)
  for i in 0..<kids.len:
    result.kids[i] = kids[i]

proc atom(k: QStmtKind): QStmt =
  new result
  result.kind = k

proc rel*(relation: int): QStmt =
  result = atom(nkRelation)
  result.rid = relation

proc `?`*(x: VarId): QStmt =
  result = atom(nkVarUse)
  result.varId = x

proc `?!`(x: VarId): QStmt =
  result = atom(nkVarDef)
  result.varId = x

proc lit*(x: Value): QStmt =
  result = atom(nkLit)
  result.v = x

proc lit*(x: int64): QStmt =
  result = lit(allocInt32(int32 x))
  result.typ.kind = tyInt32
  result.typ.size = byte sizeof(int32)
#  result.compare = cmpInt64

proc lit*(x: string): QStmt =
  result = lit(allocTempString(x))
  result.typ.kind = tyString
  result.typ.size = byte 255
#  result.compare = cmpStrings

type
  Db* = object
    relations*: array[50, BTree]

proc nested(s: seq[QStmt]): QStmt =
  result = s[0]
  var current = result
  for i in 1..<s.len:
    current.lastSon = s[i]
    current = s[i]

proc deps(n: QStmt; uses, defs: var set[VarId]) =
  ## computes the dependencies for 'n'.
  case n.kind
  of nkEmpty, nkLit, nkRelation, nkProc, nkValues, nkKeys, nkPairs: discard
  of nkVarUse:
    uses.incl(n.varId)
  of nkVarDef: defs.incl(n.varId)
  of nkPred:
    # in the pattern matching language, nkVarUse is ambiguous and can mean
    # 'nkVarDef' if previously unbound. But we can check this here easily:
    assert n.len == 3
    let subj = n[1].varId
    let obj = n[2].varId
    template defOrUse(x) =
      if x in defs: uses.incl(x)
      else: defs.incl(x)
    defOrUse(subj)
    defOrUse(obj)
  else:
    for i in 0 ..< n.len: deps(n[i], uses, defs)

proc moveup(result: QStmt; cond: QStmt) =
  var uses: set[VarId] = {}
  var defs: set[VarId] = {}
  var condUses: set[VarId] = {}
  var condDefs: set[VarId] = {}
  deps(cond, condUses, condDefs)
  doAssert condDefs == {}, "condition with side effect?"
  for i in 0..result.len-2:
    deps(result[i], uses, defs)
    if defs >= condUses:
      # all variables we care about here have been determined already.
      # This is the ideal insertion position:
      let ideal = i+1
      # make room for the condition:
      for k in countdown(result.len-1, ideal+1):
        result.kids[k] = result.kids[k-1]
      result.kids[ideal] = cond
      return

proc reorder(q: QStmt): QStmt =
  assert q.kind == nkAnd
  result = tree(nkAnd)
  ## We ensure that for every variable 'v' an nkVarDef comes before an nkVarUse
  ## (correctness). This means to move all nkPreds to the front:
  for n in q:
    if n.kind == nkPred: result.add n

  ## We also move conditions up in the list to speed up the
  ## iterations (efficiency).
  for n in q:
    if n.kind != nkPred:
      result.add nil
      moveup(result, n)

proc checkSameType(a, b: TypeDesc) =
  if a.kind == b.kind and a.id == b.id:
    discard
  else:
    quit "type mismatch"

proc annotateTypes(q: QStmt; plan: Plan) =
  case q.kind
  of nkValues, nkKeys, nkPairs, nkCall, nkAsgn, nkProc, nkRelation, nkEmpty,
     nkPred, nkCreateTable, nkInsert: discard
  of nkLit:
    assert q.typ.kind != tyNone
    q.compare = typeToCmp(q.typ.kind)
  of nkVarDef, nkVarUse:
    q.typ = plan.vartypes[q.varId]
    assert q.typ.kind != tyNone
    q.compare = typeToCmp(q.typ.kind)
  of nkFor, nkIf, nkYield, nkSelect:
    for i in 0..<q.len:
      annotateTypes(q[i], plan)
  of nkLt, nkLe, nkEq, nkNeq, nkGt, nkGe:
    annotateTypes(q[0], plan)
    annotateTypes(q[1], plan)
    checkSameType(q[0].typ, q[1].typ)
    q.typ.kind = tyBool
    q.typ.size = 1
    q.compare = q[0].compare
  of nkAnd, nkOr, nkNot:
    for i in 0..<q.len:
      annotateTypes(q[i], plan)
      if q[i].typ.kind != tyBool: quit "not of type bool"
    q.typ.kind = tyBool
    q.typ.size = 1
  of nkBetween:
    for i in 0..<q.len:
      annotateTypes(q[i], plan)
    checkSameType(q[0].typ, q[1].typ)
    checkSameType(q[1].typ, q[2].typ)
    q.typ.kind = tyBool
    q.typ.size = 1

proc compile(q: Query; db: Db; plan: Plan): QStmt =
  assert q.kind == nkSelect
  assert q.len == 2
  assert q[0].kind == nkYield
  assert q[1].kind == nkAnd
  let conds = reorder q[1]
  var res = newSeqOfCap[QStmt](conds.len+1)
  var bound: set[VarId] = {}
  for n in conds:
    case n.kind
    of nkPred:
      # a triple like '?subj pred ?obj'; pred comes first though
      # if the variable is already bound, reuse it here (implicit joins):
      let subj = n[1].varId
      let obj = n[2].varId
      var iter = atom(nkPairs)
      let pred = n[0].rid
      iter.tree = db.relations[pred]
      if subj in bound:
        if obj in bound:
          # if both are already bound, it is another 'if' constraint that
          # needs to be answered by the BTree
          doAssert false, "error: both variables already bound; not implemented"
        else:
          # XXX More efficient join here?
          iter.kind = nkValues
          iter.cond = nil
          #res.add tree(nkFor, ?!subj, ?!obj, iter, atom(nkEmpty))
      elif obj in bound:
        iter.kind = nkKeys
        iter.cond = BTreeQuery(op: opTrue)
      else:
        # XXX Collect additional constraints here!
        iter.cond = BTreeQuery(op: opTrue)
      # XXX perform type checking here!
      plan.vartypes[subj] = iter.tree.layout.keyDesc
      plan.vartypes[obj] = iter.tree.layout.valDesc
      # empty will be filled later by the 'nested' pass:
      res.add tree(nkFor, ?!subj, ?!obj, iter, atom(nkEmpty))
      incl(bound, subj)
      incl(bound, obj)
    of nkLt, nkLe, nkEq, nkNeq, nkGt, nkGe, nkAnd, nkOr, nkNot, nkBetween:
      # nice, we can leave these as they are:
      res.add tree(nkIf, n, atom(nkEmpty))
      # XXX handle nested select statements here!
    else: doAssert false, "illformed query"
  res.add(q[0])
  result = nested(res)
  annotateTypes(result, plan)

proc run*(q: Query; db: Db; a: Action) =
  var p = Plan()
  p.action = a
  let it = compile(q, db, p)
  #echo "Plan ", it
  exec(p, it)

when isMainModule:
  var pageMgr: PageMgr
  initPageMgr(addr pageMgr, 1, ".")

  proc testA() =
    const
      salary = 0
      tax = 1
      rid = VarId(0)
      sid = VarId(1)
      rsalary = VarId(2)
      ssalary = VarId(3)
      rtax = VarId(4)
      stax = VarId(5)
    let pm = addr(pageMgr)

    proc output(results: array[VarId, PinnedValue]; types: array[VarId, TypeDesc]) =
      var s = ""
      addEntry(s, results[rid].p, types[rid], pm)
      s.add " earns less than "
      addEntry(s, results[sid].p, types[sid], pm)
      s.add " but pays more taxes"
      echo s

    var db = Db()

    let strTy = TypeDesc(kind: tyString, size: 16)
    let intTy = TypeDesc(kind: tyInt32, size: 4)
    let x = pinFreshNode(pm)
    db.relations[salary] = newBTree(x.id, strTy, intTy, cmpStrings, pm)
    unpin(x)

    let y = pinFreshNode(pm)
    db.relations[tax] = newBTree(y.id, strTy, intTy, cmpStrings, pm)
    unpin(y)

    db.relations[salary].put("Angelika", 7000)
    db.relations[salary].put("Annette", 4000)
    db.relations[salary].put("Ariane", 3000)

    db.relations[tax].put("Annette", 200)
    db.relations[tax].put("Ariane", 100)
    db.relations[tax].put("Angelika", 50)

    when false:
      let myq = tree(nkSelect, tree(nkYield, ?rid, ?sid),
        tree(nkAnd,
          tree(nkPred, rel(salary), ?rid, ?rsalary),
          tree(nkPred, rel(salary), ?sid, ?ssalary),
          tree(nkLt, ?rsalary, ?ssalary),
          tree(nkPred, rel(tax), ?rid, ?rtax),
          tree(nkPred, rel(tax), ?sid, ?stax),
          tree(nkGt, ?rtax, ?stax)
        ))
    # heavily reorderd to stress the reorder pass:
    let myq = tree(nkSelect, tree(nkYield, ?rid, ?sid),
      tree(nkAnd,
        tree(nkGt, ?rtax, ?stax),
        tree(nkLt, ?rsalary, ?ssalary),
        tree(nkPred, rel(salary), ?sid, ?ssalary),
        tree(nkPred, rel(salary), ?rid, ?rsalary),
        tree(nkPred, rel(tax), ?rid, ?rtax),
        tree(nkPred, rel(tax), ?sid, ?stax)
      ))

    myq.run(db, output)


  testA()
