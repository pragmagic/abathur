
import pager, btree, vm, parser

when isMainModule:
  var pageMgr: PageMgr
  initPageMgr(addr pageMgr, 1, ".")

  const
    rid = 0
    sid = 1

  proc output(results: array[VarId, PinnedValue]; types: array[VarId, TypeDesc]) =
    let pm = addr(pageMgr)
    var s = ""
    addEntry(s, results[rid].p, types[rid], pm)
    s.add " earns less than "
    addEntry(s, results[sid].p, types[sid], pm)
    s.add " but pays more taxes"
    echo s

  proc testA() =
    let pm = addr(pageMgr)
    var attrMap = createPredicateMap()

    var db = Db()
    db.relations = @[]
    db.pm = pm
    initSchema(db)
    const query = """
table person:
  name: key(string(60))
  salary: int32
  tax: int32

insert person("Angelika", 7000, 50)
insert person("Annette", 4000, 200)
insert person("Ariane", 3000, 100)

#[
select ?rid, ?sid
where:
  ?rsalary < ?ssalary
  ?rtax > ?stax
  salary(?sid, ?ssalary)
  salary(?rid, ?rsalary)
  tax(?sid, ?stax)
  tax(?rid, ?rtax)
]#
"""
    let myq = parse(attrMap, query)
    myq.run(db, output)

  testA()
