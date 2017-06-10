
template `+!`*(x: pointer; offset: int): pointer =
  cast[pointer](cast[int](x) +% offset)
template incp*(x: pointer; offset: int) = (x = x +! offset)

proc roundup*(x, v: int): int {.inline.} =
  result = (x + (v-1)) and not (v-1)

template fetchByte*(x: pointer): byte = cast[ptr byte](x)[]
template fetchInt16*(x: pointer): int16 = cast[ptr int16](x)[]
template fetchInt32*(x: pointer): int32 = cast[ptr int32](x)[]
template fetchInt64*(x: pointer): int64 = cast[ptr int64](x)[]

template fetchFloat64*(x: pointer): float64 = cast[ptr float64](x)[]
template fetchFloat32*(x: pointer): float32 = cast[ptr float32](x)[]

template storeByte*(x: pointer; b: byte) = cast[ptr byte](x)[] = b
template storeInt32*(x: pointer; i: int32) = cast[ptr int32](x)[] = i
template storeInt64*(x: pointer; i: int64) = cast[ptr int64](x)[] = i

template storeMem*(dest, src: pointer; size: int) = copyMem(dest, src, size)
template storeMemOverlap*(dest, src: pointer; size: int) =
  moveMem(dest, src, size)

template zMem*(dest: pointer; size: int) = zeroMem(dest, size)

proc c_memcmp(a, b: pointer, size: int): cint {.
  importc: "memcmp", header: "<string.h>", noSideEffect.}

template cmpMem*(a, b: pointer; size: int): int = int(c_memcmp(a, b, size))

when false:
  # alternative cmpMem implementation:
  var i = 0
  while i < size:
    let va = fetchByte(a +! i)
    let vb = fetchByte(b +! i)
    result = int(va) - int(vb)
    if result != 0: return result
    inc i

# Note: no 'realloc' as it's problematic
template allocMem*[T](t: typedesc[T]; size: int): T = cast[T](alloc0(size))
template deallocMem*(p: pointer) = dealloc(p)
