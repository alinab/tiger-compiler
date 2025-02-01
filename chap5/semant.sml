structure Semant =
struct
  type ty = Types.ty

  structure A = Absyn
  structure E = ErrorMsg

  (*set up types for the type and variable environments *)
  type venv = Env.enventry Symbol.table
  type tenv = ty Symbol.table
  type expty = {exp: Translate.exp, ty: Types.ty}

  val venv = Env.base_venv
  val tenv = Env.base_tenv

  fun checkInt ({exp, ty}, pos) = (case ty of
                                     Types.INT => ()
                                   | _    => E.error pos "integer required")

  fun transExp(venv, tenv) =
     let fun trexp(A.NilExp) = {exp=(), ty=Types.NIL}
      | trexp(A.IntExp(i)) = {exp=(), ty=Types.INT}
      | trexp(A.StringExp(s, pos)) = {exp=(), ty=Types.STRING}
      (* Let expressions *)
      | trexp(A.LetExp{decs, body, pos}) =
        let
          val {venv=venv''', tenv=tenv'''} =
                case decs of
                        dec::rest => let val {venv=venv', tenv=tenv'} = trdec dec
                                         in trdec (List.hd rest)
                                     end
                     |  []  => {venv=venv, tenv=tenv}
           in
            transExp(venv''', tenv''') body
         end
      | trexp(_) = {exp=(), ty=Types.NIL}

    and trdec(A.TypeDec(tydecls))=
        (let
             fun enternmty ({name, ty, pos}, tenv) = Symbol.enter(tenv,
                                                                  name,
                                                                  transTy(ty))
             val tenv'' = List.foldl enternmty tenv tydecls
          in
             {venv=venv, tenv=tenv''}
          end)
       | trdec(A.VarDec{name, escape, typ=NONE, init, pos}) =
           (let val {exp, ty} = trexp init
            in
              {tenv=tenv, venv=Symbol.enter(venv, name, Env.VarEntry{ty=ty})}
            end)
       | trdec(A.VarDec{name, escape, typ=SOME(sym, posn), init, pos}) =
          (let val {exp, ty} = trexp init
               val vartyp = if ty = Types.NIL
                            then Types.RECORD([], ref())
                            else
                              case Symbol.look(tenv, sym) of
                                         SOME v => v
                                       | NONE => Types.NIL
           in
             case ty = vartyp of
               true => {tenv=tenv, venv=Symbol.enter(venv, name
                                                    , Env.VarEntry{ty=vartyp})}
             | false => {tenv=tenv, venv=venv}
           end)
       | trdec(A.FunctionDec(funcdecls)) =
       let fun mapfdecs ({name: Symbol.symbol, params: Absyn.field list
                         , result: (Symbol.symbol *  Absyn.pos) option
                         , body: Absyn.exp, pos: Absyn.pos}, venv) =
             let
               val result_ty = case result of
                           SOME(rt, pos) => let val resultTyp =
                                                case Symbol.look(tenv, rt) of
                                                     SOME v => v
                                                   | NONE => Types.NIL
                                           val {exp=_, ty=bodyTyp} =
                                                            trexp (body)
                                       in
                                         case bodyTyp = resultTyp of
                                             true => resultTyp
                                           | false => Types.NIL
                                       end
                         | NONE => Types.UNIT

                  val params' = map (fn x => case Symbol.look(tenv, #typ x) of
                                          SOME t => {name=name, ty=t}
                                        | NONE  => {name=name
                                                        , ty=Types.NIL}) params
                  val venv' = Symbol.enter(venv, name,
                                         Env.FunEntry{formals = map #ty params',
                                                      result = result_ty})
                 fun enterfparam ({name, ty}, venv) = Symbol.enter(venv, name
                                                                   , Env.VarEntry{ty=ty})
                 val venv'' = List.foldl enterfparam venv' params'
              in
                 transExp (venv'', tenv) body;
                 venv''
              end
         in
           {venv= List.foldl mapfdecs venv funcdecls, tenv=tenv}
         end

      (* Transform Absyn.Ty to Types.ty *)
      and transTy(absynty) : Types.ty =
        case absynty of
          A.NameTy(symbol, pos) =>
           let val t = case Symbol.look(tenv, symbol) of
                            SOME typ => SOME typ
                          | NONE => NONE
            in
             Types.NAME(symbol, ref(t))
            end
       | A.ArrayTy(symbol, pos) =>
           let val t = case Symbol.look(tenv, symbol) of
                                     SOME typ => SOME typ
                                   | NONE => NONE
           in
             case t of
                SOME ty => Types.ARRAY(ty, ref())
              | NONE   => Types.ARRAY(Types.NIL, ref())
           end
       | A.RecordTy(recfieldsls) =>
         let
           fun extractnmtyp recfield =
             case recfield of
                  {name, escape, typ, pos} => case Symbol.look(tenv, typ) of
                                             SOME t => (name, t)
                                          |  NONE  => (name, Types.NIL)
            val recnmtyp = List.map extractnmtyp recfieldsls
            (*fun enternmtyp ((name, ty), tenv) = Symbol.enter(tenv, name, ty)
            val tenv'' = List.foldl enternmtyp tenv recnmtyp *)
         in
           Types.RECORD(recnmtyp, ref())
         end
      in
       trexp
     end

   fun transProg abExp = transExp(venv, tenv) abExp

end
