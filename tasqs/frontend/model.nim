
import std / [times, strutils]
import karax / [jjson, jstrutils, jdict]

import knete

const
  author = "araq"
  apiUrl = "http://localhost:8080/"

type
  Connectable* = ref object of RootObj ## an element that is connectable to
                                       ## DOM elements.
    connectedTo*: Element

  TaskId* = distinct kstring
  TaskStatus* {.pure.} = enum
    ToDo, Doing, Done
  Task* = ref object of Connectable
    id*: TaskId     ## the backend ensures IDs are globally unique!
    author*: kstring
    parent*: Task
    createdAt*: kstring
    title*, desc*: kstring
    status*: TaskStatus

var
  #taskToElement = newJDict[TaskId, Element]()  # since Task IDs are not "stable",
  # such a lookup table does not work! Instead the old idea of 'of Connectable' is
  # superior.
  elementToTask = newJDict[kstring, Task]()

proc disconnect*(t: Task) =
  elementToTask.del(t.connectedTo.id)
  t.connectedTo = nil

proc connect*(t: Task; e: Element) =
  t.connectedTo = e
  #e.id = kstring(t.id)
  elementToTask[e.id] = t

proc task*(e: Element): Task = elementToTask[e.id]

proc serverTimeToStr(unixTimeStamp: int): kstring =
  &format(fromUnix(unixTimeStamp).local, "yyyy-MM-dd HH:MM")

var nextTaskId = 0 ## negative because they are not yet backed
                   ## up by the database

proc createTask*(title, desc: kstring; parent: Task; status: TaskStatus;
                 renderer: proc(t: Task)): Task =
  dec nextTaskId
  result = Task(id: TaskId(&nextTaskId), title: title, desc: desc,
    author: author, parent: parent, status: status, createdAt: "")
  let t = result
  let parentId = if t.parent != nil: t.parent.id else: TaskId(nil)

  ajaxPost(apiUrl & "create", [], toJson(%*{"author": t.author,
    "parent": kstring parentId,
    "title": t.title, "desc": t.desc, "status": &($t.status), "timestamp": 0}),
    proc(httpStatus: int, response: cstring) =
      if httpStatus == 200:
        let resp = fromJson[JsonNode](response)
        t.id = TaskId(resp["_id"].getStr)
        t.createdAt = serverTimeToStr(resp["timestamp"].getInt)
        renderer(t)
  )

proc updateTask*(t: Task; renderer: proc(t: Task)) =
  let parentId = if t.parent != nil: t.parent.id else: TaskId""
  ajaxPost(apiUrl & "update", [], toJson(%*{"parent": kstring parentId,
    "title": t.title, "desc": t.desc, "status": &($t.status),
    "_id": t.id}),
    proc(httpStatus: int, response: cstring) =
      echo "Update event ", httpStatus
  )

proc deleteTask*(t: Task) =
  ajaxPost(apiUrl & "delete", [], kstring t.id,
    proc(httpStatus: int, response: cstring) =
      echo "Delete event ", httpStatus
  )

var previousVersion: cstring

proc loadTasks*(renderer: proc(allTasks: seq[Task])) =
  ajaxPost(apiUrl & "list", [], "",
    proc(httpStatus: int, response: cstring) =
      if httpStatus >= 200 and httpStatus <= 210:
        if response == previousVersion: return
        previousVersion = response
        var allTasks: seq[Task] = @[]
        var parents: seq[TaskId] = @[]
        var idToTask = newJDict[TaskId, Task]()

        let resp = fromJson[JsonNode](response)
        for entry in resp:
          let t = Task(id: TaskId entry["_id"].getStr,
                       author: entry["author"].getStr,
                       parent: nil,
                       createdAt: serverTimeToStr(entry["timestamp"].getInt),
                       title: entry["title"].getStr,
                       desc: entry["desc"].getStr,
                       status: parseEnum[TaskStatus]($entry["status"].getStr))
          allTasks.add t
          parents.add TaskId entry["parent"].getStr
          idToTask[t.id] = t
        # now that we have all tasks, resolve the 'parent' field properly:
        for i in 0 ..< parents.len:
          allTasks[i].parent = idToTask[parents[i]]
        renderer(allTasks)
  )
