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
  fun actualTy (ty, pos) =
    (case ty of
	    Types.NAME(sym,_) => (case Symbol.look(tenv, sym) of
                                     SOME t => t
                                   | NONE => Types.UNIT)
      |  _  =>  ty)

  fun transExp(venv, tenv) =
     let fun trexp(A.NilExp) = {exp=(), ty=Types.NIL}
      | trexp(A.IntExp(i)) = {exp=(), ty=Types.INT}
      | trexp(A.StringExp(s, pos)) = {exp=(), ty=Types.STRING}
      (* Let expressions *)
      | trexp(A.LetExp{decs, body, pos}) =
       (let val {venv=venv', tenv=tenv'} =
                List.foldl (fn (x, y) => trdec x) ({venv=venv, tenv=tenv}) decs
        in
          transExp(venv', tenv') body
        end)
      | trexp(_) = {exp=(), ty=Types.NIL}

    (* Variable  declarations *)
    and trvar(A.SimpleVar(symbol, pos)) =
      (case Symbol.look(venv, symbol) of
          SOME(Env.VarEntry{ty}) => {exp=(), ty=actualTy(ty, pos)}
        | _  => (E.error pos ("undefined variable" ^ Symbol.name symbol);
                  {exp=(), ty=Types.INT}))
      | trvar (A.FieldVar(var, symbol, pos)) =
       (let
         val testvar = #ty(trvar var)
         in
           case testvar of
                Types.RECORD(_,_) => (case Symbol.look(venv, symbol) of
                                       SOME(Env.VarEntry{ty}) => {exp=(), ty=ty}
                                      | _ => (E.error pos "fielf variable not found";
                                                {exp=(), ty=Types.INT}))
               | _  =>  (E.error pos ("undefined record"); (*How to print var*)
                          {exp=(), ty=Types.INT})
         end)
      | trvar (A.SubscriptVar(var, exp, pos)) =
       (case #ty(trvar(var)) of
            Types.ARRAY(_) => let val expty = trexp exp
                                 in
                                 case (#ty expty) = #ty(trvar(var)) of
                                    true => {exp=(), ty=(#ty expty)}
                                  | false => (E.error pos ("undefined array subscript");
                                             {exp=(), ty=Types.INT})
                               end
             | _  =>  (E.error pos ("undefined array"); (*How to print var*)
                   {exp=(), ty=Types.INT}))

    (* Type declarations *)
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
                                                     SOME t => t
                                                   | NONE => Types.NIL
                                                 in resultTyp
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
                let val {exp, ty} = transExp (venv'', tenv) body
                in
                 case result_ty = ty of
                      true => venv''
                   | false => venv (* indicates an error *)
                 end
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
         in
           Types.RECORD(recnmtyp, ref())
         end
      in
       trexp
     end

   fun transProg abExp = transExp(venv, tenv) abExp

end
