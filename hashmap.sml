signature HASHMAP =
sig
  type binding
  type bucket

  exception NotFound
  type table

  val hash: string -> int
  val insert: (string * binding) -> unit
  val lookup: string -> int
  val remove: (string) -> (string * binding)
end

structure HashMap =
struct

  type binding = int
  type bucket = (string * binding) list
  exception NotFound

  type table = bucket Array.array

  val arrSize = 11     (* should be prime *)
  val hashTable : table = Array.array(arrSize, nil)

  fun hash(s: string) : int =
    CharVector.foldl (fn (c,n) => (n*256 + (ord c)) mod arrSize) 0 s

  fun insert(s:string, b: binding) =
    let val idx = hash s mod arrSize
    in
      Array.update(hashTable, idx, (s,b)::Array.sub(hashTable,idx))
    end

  fun lookup(s: string) : int =
     let val idx = hash s mod arrSize
         fun search((v',b')::rest) = if v'=s then b'
                                     else search(rest)
           | search nil = raise NotFound
     in
       search(Array.sub(hashTable, idx))
  end

  fun remove(s: string) =
   let val idx = hash s mod arrSize
       fun removeVal((a', b')::rest) = if s=a'
                                       then rest
                                       else (a',b')::removeVal(rest)
        | removeVal nil = raise NotFound
   in
     Array.update(hashTable, idx, removeVal(Array.sub(hashTable,idx)))

   end

  fun pop(s: string) =
   let val idx = hash(s) mod arrSize
       fun lastVal() = case Array.sub(hashTable, idx) of
                                   (s',b')::rest => rest
                                   | []        => []
    in
       Array.update(hashTable,idx,lastVal())
  end
end

