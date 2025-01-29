signature ENV =
sig
  (*type access*)
  type ty
  type enventry
  val base_tenv : ty Symbol.table (* predefined types *)
  val base_venv : enventry Symbol.table (* predefined functions *)
end

structure Env :> ENV =
struct
  type ty = Types.ty

  datatype enventry = VarEntry of {ty: ty}
                    | FunEntry of {formals: ty list, result: ty}

  val tyEnv = Symbol.empty : ty Symbol.table
  val vEnv  = Symbol.empty : enventry Symbol.table

  val base_tenv = Symbol.enter(tyEnv, Symbol.mksymbol "int", Types.INT)

  val base_tenv = Symbol.enter(tyEnv, Symbol.mksymbol "string", Types.STRING)


  val base_venv = Symbol.enter(vEnv, Symbol.mksymbol "print"
                                      , FunEntry{formals= [Types.STRING]
                                      , result= Types.UNIT})


   val base_venv  = Symbol.enter(vEnv, Symbol.mksymbol "flush"
                                 , FunEntry{formals= [Types.UNIT]
                                 , result= Types.UNIT})

   val base_venv  = Symbol.enter(vEnv, Symbol.mksymbol "getChar"
                                 , FunEntry{formals= [Types.UNIT]
                                            , result= Types.STRING})


   val base_venv  = Symbol.enter(vEnv, Symbol.mksymbol "ord"
                                 , FunEntry{formals= [Types.STRING]
                                            , result= Types.INT})


   val base_venv  = Symbol.enter(vEnv, Symbol.mksymbol "chr"
                                 , FunEntry{formals= [Types.INT]
                                           , result= Types.STRING})

   val base_venv  = Symbol.enter(vEnv, Symbol.mksymbol "size"
                                 , FunEntry{formals= [Types.STRING]
                                            , result= Types.INT})

   val base_venv  = Symbol.enter(vEnv , Symbol.mksymbol "substring"
                                 , FunEntry{formals= [Types.STRING
                                                      , Types.INT
                                                      , Types.INT]
                                 , result= Types.STRING})

   val base_venv  = Symbol.enter(vEnv, Symbol.mksymbol "concat"
                                 , FunEntry{formals= [Types.STRING
                                                  , Types.STRING]
                                 , result= Types.STRING})


   val base_venv  = Symbol.enter(vEnv, Symbol.mksymbol "not"
                                 , FunEntry{formals= [Types.INT]
                                           , result= Types.INT})


   val base_venv  = Symbol.enter(vEnv, Symbol.mksymbol "exit"
                                      , FunEntry{formals= [Types.INT]
                                                 , result= Types.UNIT})
end
