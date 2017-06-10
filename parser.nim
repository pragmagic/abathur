
import
  compiler / [ast, astalgo, idents, msgs, parser, renderer] # we use Nim's parser here for now.

import vm, pager

proc startsWith(n: PNode; s: string): bool =
  result = n.len >= 1 and n.kind in nkCallKinds and n[0].kind == nkIdent and
    n[0].ident.s == s

proc error(i: TLineInfo; msg: string) =
  echo i, " Error: ", msg

type
  SymTab = object
    t: TStrTable
    vars: int

{.experimental.}
using
  c: var SymTab
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
    # should be a predicate:
    var x = strTableGet(c.t, n.ident)
    if x == nil or x.kind != skProc:
      error n.info, "unknown predicate " & n.ident.s
    else:
      result = rel(x.position)
  of nkStrLit..nkTripleStrLit:
    result = lit(n.strVal)
  of nkIntLit..nkInt64Lit:
    result = lit(n.intVal)
  of nkCallKinds:
    if n[0].kind == nkIdent:
      case n[0].ident.s
      of "?":
        if n.len == 2 and n[1].kind == nkIdent:
          result = ?handleVar(c, n[1])
        else:
          error n.info, "illformed variable"
      of "<": result = tcmp(nkLt, c, n)
      of "!=": result = tcmp(nkNeq, c, n)
      of "==": result = tcmp(nkEq, c, n)
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

proc tselect*(c; s, w: PNode): QStmt =
  result = tree(nkSelect)
  result.add selDecl(c, s)
  if w.len != 2:
    error w.info, "where clause takes a single condition"
  var cond = tree(nkAnd)
  if w[1].kind == nkStmtList:
    for it in w[1]:
      cond.add texpr(c, it)
  else:
    cond.add texpr(c, w[1])
  result.add cond

var cache = newIdentCache()

proc parse*(c; s: string; filename = ""; line = 0): QStmt =
  let n = parseString(s, cache, filename, line)
  if n.kind == nkStmtList and n.len >= 2:
    var i = 0
    while i < n.len:
      if n[i].startsWith"select" and n[i+1].startsWith("where"):
        result = tselect(c, n[i], n[i+1])
        inc i
      elif n[i].kind in {TNodeKind.nkEmpty, nkCommentStmt}:
        discard
      else:
        error n[i].info, "expected a statement"
        break
      inc i
  else:
    error n.info, "expected a statement"

proc createPredicateMap*(x: varargs[(string, int)]): SymTab =
  initStrTable(result.t)
  result.vars = 0
  for a in x:
    let attr = newSym(skProc, cache.getIdent(a[0]), nil, unknownLineInfo())
    attr.position = a[1]
    strTableAdd(result.t, attr)

when isMainModule:
  var c: TStrTable
  initStrTable(c)

  discard parse(c, """
select ?foo, ?bar
where:
  ?foo == ?bar
""")
