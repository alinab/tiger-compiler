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

    and trdec(A.TypeDec[{name, ty, pos}])=
        {venv=venv, tenv=Symbol.enter(tenv, name, Types.NIL)}
       | trdec(A.VarDec{name, escape, typ=NONE, init, pos}) =
           (let val {exp, ty} = transExp(venv, tenv) init
            in {tenv=tenv, venv=Symbol.enter(venv, name, Env.VarEntry{ty=ty})}
           end)
       | trdec(A.VarDec{name, escape, typ=SOME(sym, posn), init, pos}) =
          (let val {exp, ty} = trexp init
               val vartyp = case Symbol.look(tenv, sym) of
                            SOME v => v
                          | NONE => Types.NIL
           in
             case ty = vartyp of
               true => {tenv=tenv, venv=Symbol.enter(venv, name
                                                    , Env.VarEntry{ty=vartyp})}
             | false => {tenv=tenv, venv=venv}
           end)
       | trdec(A.FunctionDec[{name, params, result = funcRes, body, pos}]) =
             (let val result_ty =
                   (case funcRes of
                      SOME(rt, pos) => let val resultTyp =
                                                case Symbol.look(tenv, rt) of
                                                     SOME v => v
                                                   | NONE => Types.NIL
                                           val {exp=_, ty=bodyTyp} = trexp body
                                       in
                                         case bodyTyp = resultTyp of
                                             true => resultTyp
                                           | false => Types.NIL
                                       end
                     | NONE => Types.UNIT)

                  val params' = map (fn x => case Symbol.look(tenv, #typ x) of
                                          SOME t => {name=name, ty=t}
                                        | NONE  => {name=name, ty=Types.NIL}) params
                  val venv' = Symbol.enter(venv, name,
                                         Env.FunEntry{formals = map #ty params',
                                                      result = result_ty})
                 fun enterfparam ({name, ty}, venv) = Symbol.enter(venv, name
                                                                   , Env.VarEntry{ty=ty})
                 val venv'' = List.foldl enterfparam venv' params'
              in trexp body;
                 {venv=venv', tenv=tenv}
              end)
      | trdec(_) = {venv=venv, tenv=tenv}
      in
       trexp
     end

   fun transProg abExp = transExp(venv, tenv) abExp

end
