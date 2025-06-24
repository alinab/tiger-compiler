structure Semant =
struct
  structure A = Absyn
  structure E = ErrorMsg
  structure Env = Env

  (*set up types for the type and variable environments *)
  type expty = {exp: Translate.exp, ty: Types.ty}



  fun checkStr ({exp, ty}, pos) = case ty of
                                     Types.STRING => ()
                                   |  _ => E.error pos "string required"


  fun existInLs (_, []) = false
      | existInLs (x, y :: ys) = x = y orelse existInLs (x, ys)

  fun checkIllCycl (typref, name, pos) =
      case typref of
        ref x => (E.error pos ("Illegal type cycle for type" ^ Symbol.name name); false)

  local
  fun transExp(venv, tenv) =
     let fun trexp(A.VarExp(var)) = trvar var
      | trexp(A.NilExp) = {exp=(), ty=Types.NIL}
      | trexp(A.IntExp(i)) = {exp=(), ty=Types.INT}
      | trexp(A.StringExp(s, pos)) = {exp=(), ty=Types.STRING}
      (* Function Calls *)
      | trexp(A.CallExp{func, args, pos}) =
        (let val (resty, formals') =
               case Symbol.look(venv, func) of
                   SOME(Env.FunEntry{formals, result}) =>
                            (case result of
                                Types.NIL => (Types.UNIT, formals)
                              |  _        => (result, formals))
                | _ => (Types.NIL, [])

             val argExpTyps =
                    List.map (fn b => actualTy (#ty (trexp b), pos)) args
             val processFormals = List.map (fn x =>
                                case x of
                                    Types.NAME(_, ref(SOME(t))) => t
                                 |  t' => t') formals'
        in
           if List.length argExpTyps = List.length processFormals
           then
               (* check that the arg and formal function types match *)
                if argExpTyps = processFormals
                then
                   let val {exp=callResExp, ty=callResTyp} =
                    List.foldl (fn (b, _) => transExp(venv, tenv) b)
                                              {exp=(), ty=Types.UNIT} args
                 in
                    {exp=(), ty=callResTyp}
                end
                else (E.error pos ("Function argument and formal types do not match");
                     {exp=(), ty=Types.NIL})
           else
             (E.error pos ("Number of formal and actual arguments do no match");
              {exp=(), ty=Types.NIL})
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
       (let val fieldExpsSyms = List.map (fn (sym, exp, p) => (sym, exp)) fields
            val evalFieldTyps = List.map (fn x =>(#1 x, #ty(trexp(#2 x)))) fieldExpsSyms
        in
         (* match the declared and evaluated type *)
            if isRecordTyp(tenv, typ)
            then let
              val SOME(rectyp) = Symbol.look(tenv, typ)
              val (Types.RECORD(recDecls, _)) = rectyp
              val validFieldTyps = List.map (fn a =>
                       case (List.find (fn x => #1 a = #1 x) recDecls) of
                            SOME (declFld, declTyp) =>
                                      (case declTyp of
                                           Types.RECORD(_,_) => if #2 a = Types.NIL
                                                               then declTyp
                                                               else #2 a
                                          | _   => #2 a)
                         |  NONE => Types.UNIT) (evalFieldTyps)

              val ty' = case (List.find (fn x => x = Types.UNIT) validFieldTyps) of
                      SOME _ =>  (E.error pos ("Mismatch in record field names");
                                  Types.UNIT)
                   |  NONE => (let val fieldSyms = List.map (fn x => #1 x) fieldExpsSyms
                                   val recordFLs = ListPair.zip(fieldSyms,
                                                                    validFieldTyps)
                               in
                                Types.RECORD(recordFLs, ref())
                               end)
            in
              {exp=(), ty=ty'}
            end
            else ((E.error pos "Initializing type is not a valid record");
                 {exp=(), ty=Types.NIL})
        end)
      (* Sequence Expressions *)
      | trexp(A.SeqExp(expls)) =
      (let val {exp=exp', ty=ty'} =
           List.foldl (fn ((e,p),_) =>
           if e = A.BreakExp(p)
           then (E.error p "Break statements must occur within a while or a for";
                {exp=(), ty=Types.UNIT})
           else trexp e)
           {exp=(), ty=Types.UNIT} expls
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
         else (E.error pos ("variable type and assigned value types do not match");
              {exp=(), ty=Types.NIL})
       end)
      (* If Expressions *)
      | trexp(A.IfExp{test, then', else'=SOME(elseExp), pos}) =
      (let
        val {exp=exp', ty=ty'} = trexp test
        val {exp=exp'', ty=ty''} =  trexp then'
        val {exp=exp''', ty=ty'''} = trexp elseExp
       in
         if ty'' = ty''' (* andalso exp' > 0 *)
         then if ty' = Types.INT
              then {exp=exp'', ty=ty''}
              else {exp=exp''', ty=ty'''}
         else {exp=(), ty=(E.error pos ("types of then - else differ");
                              Types.NIL)}
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
        if ty' = Types.INT
        then (let val {exp=exp', ty=ty'} =
                List.foldl (fn ((e,p),_) =>
                if e = A.BreakExp(p)
                then {exp=(), ty=Types.UNIT}
                else trexp e)
                {exp=(), ty=Types.UNIT} [(body,pos)]
              in
              {exp=exp', ty=ty'}
              end)
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
         then (let val {exp=exp', ty=ty'} =
                List.foldl (fn ((e,p),_) =>
                if e = A.BreakExp(p)
                then {exp=(), ty=Types.UNIT}
                else trexp e)
                {exp=(), ty=Types.UNIT} [(body,pos)]
              in
              {exp=exp', ty=ty'}
              end)
         else {exp=(), ty=Types.UNIT}
       end)
      (* Let expressions *)
      | trexp(A.LetExp{decs, body, pos}) =
       (let val {venv=venv', tenv=tenv'} =
            List.foldl (fn (x,{venv=venv'', tenv=tenv''})
                            => transDecs(venv'', tenv'', x))
                                  {venv=venv, tenv=tenv} decs
        in
          transExp(venv', tenv') body
        end)
      | trexp(A.ArrayExp{typ, size, init, pos}) =
      (let val arrSize = trexp size
           val arrInitVal = trexp init
           val {tenv=tenv', venv=venv'} =
                transDecs(venv, tenv, A.TypeDec([{name=typ,
                                                  ty=A.ArrayTy(typ, pos),
                                                  pos = pos}]))
           val arrTyp = (case Symbol.look(tenv', typ) of
                            SOME (Types.ARRAY(t,_)) => t
                          |                   _     => Types.UNIT)
       in
          {exp=(), ty=arrTyp}
       end)
      | trexp(A.BreakExp(pos)) =
            (E.error pos "Break statements must occur within a while or a for";
            {exp=(), ty=Types.UNIT})

    and compIntOrStr (expleft, expright, pos) =
        (if checkInt(trexp expleft, pos)=() andalso
                    checkInt(trexp expright, pos)=()
         then {exp=(), ty=Types.INT}
         else if checkStr(trexp expleft, pos)=() andalso
                    checkStr(trexp expright, pos)=()
              then {exp=(), ty=Types.STRING}
              else {exp=(), ty=Types.UNIT})

    and checkInt ({exp, ty}, pos) =
      (case ty of
             Types.INT => ()
           | _    => E.error pos "integer required")

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
    	    Types.NAME(sym, ref(NONE)) => (case Symbol.look(tenv, sym) of
                                         SOME ty => ty
                                       | NONE => Types.UNIT)
          | Types.NAME(sym, ref(SOME(t))) => t
          | t => t)
      in
       trexp
     end

     (* Transform Absyn.Ty to Types.ty *)
    and transTy(tenv, absynty) : Types.ty = case absynty of
         A.NameTy(symbol, pos) => (case Symbol.look(tenv, symbol) of
                                   SOME typ => Types.NAME(symbol, ref(SOME(typ)))
                                 | NONE => Types.NAME(symbol, ref NONE))
       | A.ArrayTy(symbol, pos) =>
                        (case Symbol.look(tenv, symbol) of
                               SOME typ => Types.ARRAY(typ, ref())
                             | NONE  => (E.error pos ("array type does not exist");
                                          Types.NIL))
      | A.RecordTy(recfieldsls) =>
         (let fun extractRecTyp recfield =
                case recfield of {name, escape, typ, pos} =>
                            case Symbol.look(tenv, typ) of
                              SOME t => (name, t)
                            | NONE  => (E.error pos ("record type does not exist");
                                        (name, Types.NIL))
                val recresult = List.map extractRecTyp recfieldsls
             in
             Types.RECORD(recresult, ref())
         end)

    and getFuncResTyp(venv, tenv, result) =
         case result of
             SOME(rt, pos) => (case Symbol.look(tenv, rt) of
                                     SOME rty => rty
                                  | NONE => Types.NIL)
          | NONE => Types.NIL

    and isRecordTyp(tenv, t) =
        case Symbol.look(tenv, t) of
             SOME(Types.RECORD(_, ref())) => true
           | _                          => false

    (* Type declarations *)
    and transDecs(venv, tenv, A.TypeDec(tydecls))=
       (let fun enterTyDec ({name, ty, pos}, tenv) =
            case ty of
              A.RecordTy(fieldls) =>
                let val tenv'' = Symbol.enter(tenv, name,
                                            Types.NAME(name, ref NONE))
                    in
                    Symbol.update(tenv'', name,
                                    Types.NAME(name, ref(SOME(transTy(tenv'', ty)))))
                end
            | _ =>   Symbol.enter(tenv, name, transTy(tenv, ty))
            val tenv''' = List.foldl enterTyDec tenv tydecls
            in
             {venv=venv, tenv=tenv'''}
          end)
       | transDecs(venv, tenv, A.VarDec{name, escape, typ=NONE, init, pos}) =
           (let val {exp=exp', ty=ty'} = transExp(venv, tenv) init
            in
              {tenv=tenv, venv=Symbol.enter(venv, name, Env.VarEntry{ty=ty'})}
            end)
       | transDecs(venv, tenv, A.VarDec{name, escape, typ=SOME(symbol, p),
                                                      init, pos}) =
          (let val typConstr = case Symbol.look(tenv, symbol) of
                               SOME(Types.NAME(_, ref(SOME(t)))) => t
                             | SOME t => t
                             | NONE => Types.NIL
               val {exp=exp', ty=ty'} = transExp(venv, tenv) init
           in
             {tenv=tenv , venv=Symbol.enter(venv, name,
                                                 Env.VarEntry{ty=ty'})}
           (* case ty' = typConstr of
             false =>  Initializing expressions of type NIL must be constrained
                       * by a record type
                      if typConstr = Types.NIL
                      then {tenv=tenv , venv=Symbol.enter(venv, name,
                              Env.VarEntry{ty=Types.RECORD([], ref())})}
                      else
                      {tenv=tenv , venv=Symbol.enter(venv, name,
                                                 Env.VarEntry{ty=ty'})}
          | true  => (E.error pos ("variable type and initialization value  \
                                   \  types do not match");
                       {tenv=tenv, venv=venv})
           *)
           end)
       | transDecs(venv, tenv, A.FunctionDec(fundecls)) =
          (let fun mapFunDec (name, params, result, venv) =
             let
               val resultTy = getFuncResTyp(venv, tenv, result)

                val params' = List.map (fn p as {name, escape, typ, pos}
                               => case Symbol.look(tenv, typ) of
                                 SOME t => Types.NAME(name, ref(SOME(t)))
                               | NONE  => Types.NAME(name, ref NONE)) params

              in
                Symbol.enter(venv, name, Env.FunEntry{formals = params',
                                                      result = resultTy})
              end
           in
             let val venv'' = List.foldl (fn (f, v) =>
                                    mapFunDec(#name f, #params f, #result f, v))
                                    venv fundecls
             in
              let
                val funcResTyp = getFuncResTyp(venv'', tenv, #result (List.hd fundecls))

                val venv''' =
                  let val prms = List.concat (List.map (fn b => #params b) fundecls)
                      val nameTypVals = List.map (fn x =>
                          let val t = case Symbol.look(tenv, #typ x) of
                              SOME t' => t'
                            | NONE  => Types.UNIT
                          in
                          (#name x, t)
                          end) prms
                  in
                    List.foldl (fn ((n,t), v) =>
                        Symbol.enter(v, n, Env.VarEntry{ty=t})) venv'' nameTypVals
                  end

                val bodies = List.map (fn b => #body b) fundecls
                val {exp=exp', ty=ty'} = List.foldl (fn (b, _)
                                           => transExp(venv''', tenv) b)
                                                   {exp=(), ty=Types.UNIT} bodies

                val funcPos = #pos (List.hd fundecls)
                in
                  if funcResTyp = Types.NIL
                  then {venv=venv''', tenv=tenv}
                  else if ty' = funcResTyp
                        then {venv=venv''', tenv=tenv}
                        else
                        (E.error funcPos "Declared and actual result for function\
                                     \ does not match";
                        {venv=venv, tenv=tenv})
               end
             end
         end)


  in fun transProg abExp = transExp(Env.base_venv, Env.base_tenv) abExp
  end
end
