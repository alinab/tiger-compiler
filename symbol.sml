signature SYMBOL =
sig
  eqtype symbol
  exception Symbol

  val mksymbol : string -> symbol
  val name : symbol -> string

  type 'a table

  val empty : 'a table
  val isEmpty: 'a table -> bool
  val enter : 'a table * symbol * 'a -> 'a table
  val remove: 'a table * int -> 'a table * 'a
  val look : 'a table * symbol -> 'a option
  val update : 'a table * symbol * 'a -> 'a table
end

structure Symbol :> SYMBOL =
struct
  type symbol = (string * int)

  exception Symbol
  val hashtable : (string, int) HashTable.hash_table =
    HashTable.mkTable (HashString.hashString, (op =)) (128, Symbol)
  val nextsymval = ref 0

  fun mksymbol name =
     case HashTable.find hashtable name
       of SOME i => (name, i)
        | NONE => let val i = !nextsymval
                  in
                      nextsymval := i + 1;
                      HashTable.insert hashtable (name, i);
                      (name, i)
                  end

  fun name(s,n) = s
  fun symval(s,n) = n

  (* Use a IntBinaryMap for inserting symbols into a balanced
   * search tree to be used as a functional-style environment *)

  type 'a table = 'a IntBinaryMap.map

  val empty = IntBinaryMap.empty : ('a table)

  fun isEmpty(t: 'a table) = IntBinaryMap.isEmpty(t)
  fun enter(t: 'a table, (s,n): symbol, a: 'a) = IntBinaryMap.insert(t,n,a)
  fun remove(t: 'a table, k: int) = IntBinaryMap.remove(t, k)
  fun look(t: 'a table, (s,n): symbol) = IntBinaryMap.find(t,n)
  fun update(t: 'a table, (s,n): symbol, a: 'a) = IntBinaryMap.insert'((n, a), t)

end
