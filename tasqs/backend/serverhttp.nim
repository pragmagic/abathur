

import asynchttpserver, asyncdispatch, asyncfile, mimetypes, os, json, arangodb

proc error(msg: string) = echo "[Error] ", msg
proc warn(msg: string) = echo "[Warning] ", msg
proc hint(msg: string) = echo "[Hint] ", msg

proc main*() =
  let httpServer = newAsyncHttpServer()
  let dbClient = connect()

  proc reqhandler(req: Request) {.async.} =
    let path = req.url.path
    hint "URL " & path
    var err: HttpCode = Http200
    if '.' in path:
      let contentType = getMimetype(newMimetypes(), splitFile(path).ext)
      let headers = newHttpHeaders([("Content-Type", contentType)])
      var file: AsyncFile
      var data = ""
      try:
        file = openAsync("frontend" / path, fmRead)
      except OSError, IOError:
        err = Http404
      if err != Http404:
        data = await file.readAll()
      await req.respond(err, data, headers)
      if err != Http404:
        file.close()
    else:
      let headers = newHttpHeaders([("Content-Type","application/json")])
      let rawBody = req.body
      hint "Got raw data " & rawBody
      var resp: JsonNode = nil
      case path
      of "/create":
        resp = dbClient.insertEntry(rawBody)
      of "/update":
        let j = parseJson(rawBody)
        let id = j{"_id"}.getStr("")
        if id.len >= 0:
          err = dbClient.updateEntry(id, rawBody)
      of "/list":
        resp = dbClient.listEntries()
        echo "RESP IS ", resp
      of "/read":
        resp = dbClient.readEntry(rawBody)
      of "/delete":
        err = dbClient.deleteEntry(rawBody)
      else:
        err = Http404

      if not resp.isNil:
        await req.respond(Http200, $resp, headers)
      else:
        await req.respond(err, "")

  waitFor httpServer.serve(Port(8080), reqhandler)

main()