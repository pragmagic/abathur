#[
Karax DB: Design

TODO:
  - finish query transformator
  - allow to declare new BTree relations
  - port Nim lexer/parser to create real query language
  - patch Nim JS codegen to produce efficient packed
    memory layout using TypedArrays.
  - make transactor.nim use Karax DB.


Interesting self-query:

SELECT r.id, s.id
FROM Employees r, Employees s
WHERE r.salary < s.salary AND r.tax > s.tax;

select ?rid, ?sid
where
  ?rsalary :salary ?rid   # this defines an execution order
  ?ssalary :salary ?sid
  ?rsalary < ?ssalary
  ?rtax :tax ?rid
  ?stax :tax ?sid
  ?rtax > ?stax

SELECT r.id, s.id
FROM Events r, Events s
WHERE r.start <= s.end AND r.end >= s.start
AND r.id != s.id;
]#

# All data is stored as triples. We use 3 covering indexes (BTrees)
# to store everything:
#
# (subj, pred, obj)
# The query language looks the same but supports ?variables too to
# support joins. The predicates `==`, `<`, `>`, `>=`, `<=` as well
# as ``between(x, a, b)`` are builtin and mapped to efficient BTree
# queries.
#[
Queries are translated into Nim code at compile-time if used as a
Nim DSL or else are compiled into an AST.

]#

import marshal, streams, os
import pager, btree

# We need SPO, POS, OSP, OPS
# Subject: int64 (or flexible typed)
# Predicate: int32
# Object: dynamically typed, but always the same type
#
# Type system: declare for every predicate the subject and object types.

# Btrees: SPO (covering index)
# POS  (Predicate object subject)
# PSO  (Predicate subject object)

# OSP  (object subject predicate?) --> not required
# OPS  (object predicate subject)  --> not required

# SPO as a key requires 2 type descs and 3 payloads. As the value we can
# then store the valid flag as well as the logical timestamp.
# --> covering index means we don't have duplicate keys.

type
  RelId = distinct int32
  Rel = object
    id: RelId
    keyType, valType: TypeDesc
    name: string
  IndexRel = object
    id: RelId
    offsets: seq[byte] ## offsets into the Rel we seek to index
                       ## but for partial indexes we need to be
                       ## able to store (byte; ValueA..ValueB) information
                       ## too complex solution
                       ## --> instead: use an 'on insert trigger'.
  DbDesc = object
    maxPageId: PageId
    rels: seq[Rel]

  Db = object
    desc: DbDesc
    pm: PageMgr
#    indexes: seq[IndexRel]

proc error(msg: string) = quit(msg)

proc open*(db: var Db; dir: string; maxMem = 1024 * 1024 * 32) =
  let root = newFileStream(dir / rootFilename, fmRead)
  if root != nil:
    marshal.load(root, db.desc)
    root.close()
  else:
    db.desc.maxPageId = 1
    db.desc.rels = @[]
  initPageMgr(addr db.pm, db.desc.maxPageId, dir, maxMem)

proc close*(db: var Db) =
  db.desc.maxPageId = db.pm.maxPageId
  storeDirtyPages(addr db.pm)
  let root = newFileStream(db.pm.dir / rootFilename, fmWrite)
  if root != nil:
    marshal.store(root, db.desc)
    root.close()
  else:
    error "cannot store root data structure"
