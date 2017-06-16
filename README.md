# abathur
A genetically modified database

# Todo
- 'insert' needs to allow inserting from a select statement
- nested 'select' queries need to be supported
- 'delete' needs to be implemented
- implement 'index' declarations and partial indexes
- implement nominal types
- implement time handling routines
- implement integer handling routines
- implement string handling routines
- select count, min, max support
- perform integer conversions
- check primary keys are declared correctly
- optimize the case: key is "N, N+1, N+2, N+3, ..."
- implemented 'order by', 'group by'
- finish query transformator
- patch Nim JS codegen to produce efficient packed
  memory layout using TypedArrays.
- make transactor.nim use Abathur.
