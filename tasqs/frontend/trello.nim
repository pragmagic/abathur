
import knete, widgets, model
import karax / [jstrutils, kajax]

template bindField*(t, field): Element =
  editable t.field, proc (value: kstring) =
    t.field = value
    updateTask t, proc (t: Task) =
      discard "we already updated the UI"

proc renderTask(t: Task): Element =
  proc dodelete =
    deleteTask t
    delete result

  proc dragstart(ev: Event) =
    ev.prepareDragData("taskid", ev.target.id)

  result = buildHtml():
    tdiv(draggable="true", ondragstart=dragstart, id = cstring(t.id)):
      bindField t, title
      br()
      bindField t, desc
      br()
      bold:
        text t.author
      button(onclick = dodelete):
        text cross
  connect(t, result)

proc open(e: Element) =
  e.style.display = "block"

proc newTaskDialog(): Element =
  var titleInp = buildHtml():
    input(setFocus = true)
  var descInp = buildHtml():
    input()

  proc close =
    result.style.display = "none"

  proc submit =
    let t = createTask(titleInp.value, descInp.value, nil, ToDo, proc (t: Task) =
      t.connectedTo.replace renderTask(t))
    close()
    getElementById("ToDoColumn").add renderTask(t)

  result = buildHtml():
    tdiv(style={display: "none", position: "fixed",
        left: "0", top: "0", width: "100%", height: "100%",
        overflow: "auto",
        backgroundColor: "rgb(0,0,0)",
        backgroundColor: "rgba(0,0,0,0.4)", zIndex: "1"}):
      tdiv(style={backgroundColor: "#fefefe",
          margin: "15% auto", padding: "20px", border: "1px solid #888",
          width: "80%"}):
        span(onclick = close, style={color: "#aaa",
            cssFloat: "right",
            fontSize: "28px",
            fontWeight: "bold"}):
          text cross
        p:
          text "Task name"
          titleInp
        p:
          text "Task description"
          descInp
        span(onclick = submit, style={color: "#0f0",
            cssFloat: "left",
            fontWeight: "bold"}):
          text "Submit"

var dialog = newTaskDialog()

proc moveTask(t: Task; s: TaskStatus) =
  t.status = s
  updateTask t, proc (t: Task) =
    discard "we already updated the UI"

proc tasks(c: TaskStatus; allTasks: seq[Task]): seq[Task] =
  result = @[]
  for t in allTasks:
    if t.status == c and t.parent == nil:
      result.add t

proc renderColumn(allTasks: seq[Task]; c: TaskStatus): Element =
  proc doadd(): proc () =
    result = proc() = dialog.open()

  proc allowDrop(ev: Event) = ev.preventDefault()
  proc drop(ev: Event) =
    ev.preventDefault()
    let el = getElementById ev.recvDragData("taskid")

    moveTask(el.task, c)
    ev.target.up("mycolumn").add(el)

  result = buildHtml():
    tdiv(class = "mycolumn", style = {cssFloat: "left", width: "25%"},
         ondrop = drop, ondragover = allowDrop, id = $c & "Column"):
      tdiv(class = "myheader"):
        span:
          text $c
        span(onclick = doadd()):
          text plus
      for t in c.tasks(allTasks):
        renderTask(t)

proc renderBoard(allTasks: seq[Task]) =
  let body = buildHtml(tdiv):
    dialog
    for c in ToDo..Done:
      renderColumn(allTasks, c)
  replaceById body

proc periodically() =
  loadTasks renderBoard

proc main(hashPart: kstring): Element =
  setInterval periodically, 500
  result = buildHtml():
    tdiv()

setInitializer main
