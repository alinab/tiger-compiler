structure Semant =
struct
  structure A = Absyn
  structure E = ErrorMsg
  structure Env = Env

  (*set up types for the type and variable environments *)
  type expty = {exp: Translate.exp, ty: Types.ty}


  fun checkInt ({exp, ty}, pos) = (case ty of
                                     Types.INT => ()
                                   | _    => E.error pos "integer required")

  fun checkStr ({exp, ty}, pos) = case ty of
                                     Types.STRING => ()
                                   |  _ => E.error pos "string required"


  local
  fun transExp(venv, tenv) =
     let fun trexp(A.VarExp(var)) = trvar var
      | trexp(A.NilExp) = {exp=(), ty=Types.NIL}
      | trexp(A.IntExp(i)) = {exp=(), ty=Types.INT}
      | trexp(A.StringExp(s, pos)) = {exp=(), ty=Types.STRING}
      (* Function Calls *)
      | trexp(A.CallExp{func, args, pos}) =
        (let
          val (funcResTy, funcFormals) =
            case Symbol.look(venv, func) of
                        SOME(Env.FunEntry{formals,result}) => (result, formals)
                     |  _ =>  (E.error pos "function name is missing";
                              (Types.UNIT, []))
         val resultTypes = List.map (fn x => #ty (transExp(venv, tenv) x)) args
        in
          if resultTypes = funcFormals
          then {exp=(), ty=funcResTy}
          else {exp=(), ty=Types.UNIT}
        end)
      (* Expressions with operations *)
      | trexp(A.OpExp{left, oper=A.PlusOp, right, pos}) =
        (checkInt(trexp left, pos);
        checkInt(trexp right, pos);
        {exp=(), ty=Types.INT})
      | trexp(A.OpExp{left, oper=A.MinusOp, right, pos}) =
        (checkInt(trexp left, pos);
        checkInt(trexp right, pos);
        {exp=(), ty=Types.INT})
      | trexp(A.OpExp{left, oper=A.TimesOp, right, pos}) =
        (checkInt(trexp left, pos);
        checkInt(trexp right, pos);
        {exp=(), ty=Types.INT})
      | trexp(A.OpExp{left, oper=A.DivideOp, right, pos}) =
        (checkInt(trexp left, pos);
        checkInt(trexp right, pos);
        {exp=(), ty=Types.INT})
      (* Comparisions between pairs of integers or pairs of strings*)
      | trexp(A.OpExp{left, oper=A.EqOp, right, pos}) =
        compIntOrStr(left, right, pos)
      | trexp(A.OpExp{left, oper=A.NeqOp, right, pos}) =
        compIntOrStr(left, right, pos)
      | trexp(A.OpExp{left, oper=A.LtOp, right, pos}) =
        compIntOrStr(left, right, pos)
      | trexp(A.OpExp{left, oper=A.LeOp, right, pos}) =
        compIntOrStr(left, right, pos)
      | trexp(A.OpExp{left, oper=A.GtOp, right, pos}) =
        compIntOrStr(left, right, pos)
      | trexp(A.OpExp{left, oper=A.GeOp, right, pos}) =
        compIntOrStr(left, right, pos)
      (* Record Expressions *)
      | trexp(A.RecordExp{fields, typ, pos}) =
       (let
         val fieldExpsSyms =
            List.map (fn (sym, exp, p) => (sym, exp)) fields
         val fieldTyps = List.map (fn x => #ty(trexp(#2 x))) fieldExpsSyms
         val fieldSyms = List.map (fn x => #1 x) fieldExpsSyms
         val recordFLs = ListPair.zip(fieldSyms, fieldTyps)
        in
          {exp=(), ty=Types.RECORD(recordFLs, ref())}
        end)
      (* Sequence Expressions *)
      | trexp(A.SeqExp(expls)) =
      (let
         val {exp=exp', ty=ty'} =
           List.foldl (fn ((e,p),_) => trexp e) {exp=(), ty=Types.UNIT} expls
       in
         {exp=exp', ty=ty'}
       end)
      (* Assignment Expressions *)
      | trexp(A.AssignExp{var, exp, pos}) =
      (let
        val {exp=exp'', ty=ty''} = trvar var
        val {exp=exp', ty=ty'} = transExp(venv, tenv) exp
       in
         if ty' = ty''
         then {exp=(), ty=ty'}
         else {exp=(), ty=Types.UNIT}
       end)
      (* If Expressions *)
      | trexp(A.IfExp{test, then', else'=SOME(elseExp), pos}) =
      (let
        val {exp=exp', ty=ty'} = trexp test
        val {exp=exp'', ty=ty''} =  trexp then'
        val {exp=exp''', ty=ty'''} = trexp elseExp
       in
         if ty' = Types.INT andalso ty'' = ty''' (* andalso exp' > 0 *)
         then {exp=exp'', ty=ty''}
         else {exp=exp''', ty=ty'''}
       end)
      | trexp(A.IfExp{test, then', else'=NONE, pos}) =
      (let
        val {exp=exp', ty=ty'} = trexp test
        val {exp=exp'', ty=ty''} =  trexp then'
       in
         if ty' = Types.INT (* andalso exp' > 0 *)
         then {exp=exp'', ty=ty''}
         else {exp=exp', ty=ty'}
       end)
      (* While Expressions *)
      | trexp(A.WhileExp{test, body, pos}) =
      (let
        val {exp=exp', ty=ty'} = trexp test
       in
        if ty' = Types.INT (* andalso exp' > 0 *)
        then let val {exp=exp'', ty=ty''} = trexp body
             in
               if ty'' = Types.UNIT
               then trexp(A.WhileExp{test=test, body=body, pos=pos})
               else {exp=exp'', ty=ty''}
             end
        else {exp=(), ty=Types.UNIT}
       end)
      (* For Expressions *)
      | trexp(A.ForExp{var, escape, lo, hi, body, pos}) =
      (let
        val {venv=venv', tenv=tenv'} = transDecs(venv, tenv, A.VarDec{name=var
                                            , escape=escape
                                            , typ=NONE
                                            , init = lo, pos = pos})
        val {exp=expLo, ty=tyLo} = trexp lo
        val {exp=expHi, ty=tyHi} = trexp hi
       in
         if tyLo = tyHi andalso tyLo = Types.INT
         (* check for actual values later *)
         then transExp(venv', tenv') body
         else {exp=(), ty=Types.UNIT}
       end)
      | trexp(A.BreakExp(pos)) = ( {exp=(), ty=Types.NIL} )
      (* Let expressions *)
      | trexp(A.LetExp{decs, body, pos}) =
       (let val {venv=venv', tenv=tenv'} =
        List.foldl (fn (x,{venv=venv'', tenv=tenv''})
                            => transDecs(venv'', tenv'', x))
                                  {venv=Env.base_venv, tenv=Env.base_tenv} decs
        in
          transExp(venv', tenv') body
        end)
      | trexp(A.ArrayExp{typ, size, init, pos}) =
      (let val arrSize = trexp size
           val arrInitVal = trexp init
           val {tenv=tenv', venv=venv'} = transDecs(venv, tenv
                                            , A.TypeDec([{name=typ
                                            , ty=A.ArrayTy(typ, pos)
                                            , pos = pos}]))
           val arrTyp = (case Symbol.look(tenv', typ) of
                            SOME (Types.ARRAY(t,_)) => t
                          | _     => Types.UNIT)
       in
          {exp=(), ty=arrTyp}
       end)

    and compIntOrStr (expleft, expright, pos) =
        (if checkInt(trexp expleft, pos)=() andalso
                    checkInt(trexp expright, pos)=()
         then {exp=(), ty=Types.INT}
         else if checkStr(trexp expleft, pos)=() andalso
                    checkStr(trexp expright, pos)=()
              then {exp=(), ty=Types.STRING}
              else {exp=(), ty=Types.UNIT})

    (*** Expression checking ends here ***)

    (* Variable lookups *)
    and trvar(A.SimpleVar(symbol, pos)) =
      (case Symbol.look(venv, symbol) of
          SOME(Env.VarEntry{ty}) => {exp=(), ty=actualTy(ty, pos)}
           | _                   => (E.error pos ("undefined variable: "
                                                        ^ Symbol.name symbol);
                                    {exp=(), ty=Types.UNIT}))
      | trvar(A.FieldVar(var, symbol, pos)) =
      (case #ty(trvar var) of
          Types.RECORD(symtyls, u) =>
            (case (List.find (fn (sym, ty) => symbol = sym) symtyls) of
                 SOME (s,t) => {exp=(), ty=t}
               | NONE =>  (E.error pos ("undefined record field");
                          {exp=(), ty=Types.UNIT}))
           |   _  =>  (E.error pos ("undefined record");
                      {exp=(), ty=Types.UNIT}))
      | trvar(A.SubscriptVar(var, exp, pos)) =
       (case #ty(trvar var) of
          Types.ARRAY(ty, _) => let val expty = trexp exp
                                in
                                    {exp=(), ty=(#ty expty)}
                                end
         | _  =>  (E.error pos ("undefined array"); (*How to print var*)
                  {exp=(), ty=Types.UNIT}))


      and actualTy (ty, pos) =
        (case ty of
    	    Types.NAME(sym, ref(SOME(t))) => (case Symbol.look(tenv, sym) of
                                         SOME ty => ty
                                       | NONE => Types.UNIT)
          | t => t)
      in
       trexp
     end

     (* Transform Absyn.Ty to Types.ty *)
    and transTy(tenv, absynty) : Types.ty =
        case absynty of
          A.NameTy(symbol, pos) => let val t = case Symbol.look(tenv, symbol) of
                                                 SOME typ => typ
                                                | NONE => Types.UNIT
                                            in
                                        Types.NAME(symbol, ref(SOME(t)))
                                        end
       | A.ArrayTy(symbol, pos) =>
           (case Symbol.look(tenv, symbol) of
                   SOME typ => Types.ARRAY(typ, ref())
                | NONE => Types.UNIT)
       | A.RecordTy(recfieldsls) =>
         let
           fun extractnmtyp recfield =
             case recfield of
                  {name, escape, typ, pos} => case Symbol.look(tenv, typ) of
                                          SOME t => (name, t)
                                        | NONE  => (name, Types.NIL)
            val recnmtyp = List.map extractnmtyp recfieldsls
         in
           Types.RECORD(recnmtyp, ref())
         end

    (* Type declarations *)
    and transDecs(venv, tenv, A.TypeDec(tydecls))=
        (let
             fun enternmty ({name, ty, pos}, tenv) =
                            Symbol.enter(tenv, name, transTy(tenv, ty))
             val tenv' = List.foldl enternmty tenv tydecls
          in
             {venv=venv, tenv=tenv'}
          end)
       | transDecs(venv, tenv, A.VarDec{name, escape, typ=NONE, init, pos}) =
           (let val {exp=exp', ty=ty'} = transExp(venv, tenv) init
            in
              {tenv=tenv, venv=Symbol.enter(venv, name, Env.VarEntry{ty=ty'})}
            end)
       | transDecs(venv, tenv, A.VarDec{name, escape, typ=SOME(symbol, p), init, pos}) =
          (let val {exp=exp', ty=ty'} = transExp(venv, tenv) init
               val typcons = case Symbol.look(tenv, symbol) of
                                         SOME v => v
                                       | NONE => Types.NIL
           in case ty' = typcons of
              true => {tenv=tenv, venv=Symbol.enter(venv, name
                                                    , Env.VarEntry{ty=typcons})}
          |   false => {tenv=tenv, venv=Symbol.enter(venv , name
                                                  , Env.VarEntry{ty=Types.NIL})}
           end)
       | transDecs(venv, tenv, A.FunctionDec(funcdecls)) =
        (let fun mapfdecs ({name: Symbol.symbol, params: Absyn.field list
                         , result: (Symbol.symbol *  Absyn.pos) option
                         , body: Absyn.exp, pos: Absyn.pos}, vEnv) =
             let
               val result_ty = case result of
                   SOME(rt, pos) => (case Symbol.look(tenv, rt) of
                                          SOME t => t
                                        | NONE  => Types.NIL)
                  | NONE => Types.NIL

                val params' = List.map (fn {name, escape, typ, pos} =>
                                                case Symbol.look(tenv, typ) of
                                          SOME t => {name=name, ty=t}
                                        | NONE  => {name=name , ty=Types.NIL})
                                        params

                val venv' = Symbol.enter(vEnv, name,
                                          Env.FunEntry{formals = List.map #ty params',
                                                       result = result_ty})

                fun enterparam ({name, ty}, venv) =
                            Symbol.enter(venv, name, Env.VarEntry{ty=ty})

                val venv'' = List.foldl enterparam venv' params'
              in
                transExp(venv'', tenv) body;
                venv''
             end
         in
           let val venv''' = List.foldl (fn (x,v) => mapfdecs(x, v)) venv funcdecls
           in
            {venv=venv''', tenv=tenv}
           end
         end)

  in fun transProg abExp = transExp(Env.base_venv, Env.base_tenv) abExp
  end
end
