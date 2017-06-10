type
  TripleHeader = object   # 8 bytes
    s: array[3, byte]     # sizes; size = 255 means overflow
    t: array[3, TypeKind] # types of the triples
    padding: array[2, byte]

const
  tripleAlign = 8
  sizeOverflow = 255

proc allocTriple*(sizeA, sizeB, sizeC: int): SepValue =
  template fullSize(size): untyped =
    roundup(if size >= sizeOverflow: size + sizeof(int32) + sizeof(int64)
            else: size,
            tripleAlign)
  let a = fullSize(sizeA)
  let b = fullSize(sizeB)
  let c = fullSize(sizeC)
  result = allocMem(SepValue, a + b + c + sizeof(TripleHeader))

proc putI*(triple: SepValue; i: range[0..2];
           typ; TypeDesc; val: pointer; size: int) =
  let r = cast[ptr TripleHeader](triple)
  r.t[i] = typ.kind
  var offset = sizeof(TripleHeader)
  for j in 0..<i:
    let size = if r.s[i] == sizeOverflow:
                 fetchInt32(r !+ offset).int
               else:
                 int(r.s[i])
    offset += roundup(size, sizeOverflow)
  if size < sizeOverflow:
    r.s[i] = byte size
    storeMem(r +! offset, val, size)
  else:
    r.s[i] = sizeOverflow
    storeInt32(r +! offset, size.int32)
    # page ID = 0 means "data embedded":
    storeInt64(r +! (offset + sizeof(int32)), 0)
    storeMem(r +! (offset + sizeof(int32) + sizeof(int64)), val, size)

proc deallocTriple*(p: SepValue) =
  deallocMem(pointer p)

proc storeTriple(p: pointer; size: int; s: pointer; slen: int32;
                 pm: Pager) =
  assert size >= 16 and size <= 256
  if slen < size:
    # store as a short string:
    storeByte(p, byte(slen))
    storeMem(p +! 1, s, slen)
  else:
    storeByte(p, 255)
    storeInt32(p +! 4, slen)
    let z = storeVla(pm, s, slen)
    storeInt64(p +! 8, int64(z))
