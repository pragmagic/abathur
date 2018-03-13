
import httpclient, times, strutils, json

const baseUrl = "http://127.0.0.1:8529/"

proc getJwt(): string =
  let h = newHttpClient()
  let resp = h.request(baseUrl & "_open/auth", httpMethod = HttpPost, body = """{"username":"root","password":"araq123"}""")
  if resp.status == Http200:
    let token = parseJson(resp.body)
    result = token{"jwt"}.getStr
  close(h)

proc connect*(): HttpClient =
  let jwt = getJwt()
  result = newHttpClient()
  result.headers = newHttpHeaders({"Authorization": "bearer " & jwt, "Content-Type": "application/json"})

proc insertEntry*(h: HttpClient; rawEntry: string): JsonNode =
  let entry = parseJson(rawEntry)
  entry["timestamp"] = %toUnix(getTime())
  let resp = h.request(baseUrl & "/_db/board/_api/document/Entries?returnNew=true", httpMethod = HttpPost,
      body = $entry)
  if resp.status == Http202:
    result = parseJson(resp.body)["new"]
  echo "insert entry ", resp.status

proc listEntries*(h: HttpClient): JsonNode =
  when false:
    let resp = h.request(baseUrl & "/_db/board/_api/simple/all", httpMethod = HttpPut,
        body = """{"collection": "Entries"}""")
  let resp = h.request(baseUrl & "/_db/board/_api/cursor", httpMethod = HttpPost,
      body = """{"query": "FOR e IN Entries SORT(e.timestamp) RETURN e", "count": true, "batchSize": 100}""")
  if resp.status == Http201:
    result = parseJson(resp.body)["result"]
    # XXX Handle 'hasMore' here?

proc readEntry*(h: HttpClient; id: string): JsonNode =
  let resp = h.request(baseUrl & "/_db/board/_api/document/" & id, httpMethod = HttpGet)
  if resp.status == Http200:
    result = parseJson(resp.body)

proc updateEntry*(h: HttpClient; id, entry: string): HttpCode =
  let resp = h.request(baseUrl & "/_db/board/_api/document/" & id, httpMethod = HttpPatch,
      body = entry)
  result = resp.code

proc deleteEntry*(h: HttpClient; id: string): HttpCode =
  let resp = h.request(baseUrl & "/_db/board/_api/document/" & id, httpMethod = HttpDelete)
  result = resp.code

when false:
  proc testInsert() =
    let h = connect(getJwt())
    let resp = h.request(baseUrl & "/_db/board/_api/document/Entries", httpMethod = HttpPost,
        body = """{"name":"Test","desc":"Some description here","timestamp": $1}""" % $toUnix(getTime()))
    echo resp.status


  deleteEntry(connect(getJwt()), "18973")

  testInsert()
