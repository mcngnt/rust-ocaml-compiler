open Luaparser.Ast
type value = Value.t
type coroutine = Value.coroutine
type env = Value.env

let create_scope (names: string list) (values: value list) : (name, value) Hashtbl.t =
  let ht = Hashtbl.create (List.length names) in
  List.iter2 (fun n v -> Hashtbl.add ht n v) names values;
  ht

let rec interp_block (env : env) (blk : block) (k : value -> unit) (co : coroutine) : unit =
  let new_scope = create_scope blk.locals (List.map (fun x -> Value.Nil) blk.locals) in
  let nenv = Value.{ globals = env.globals; locals = new_scope::env.locals } in
  interp_stat nenv (blk.body) (fun v -> interp_exp nenv (blk.ret) k co) co

and interp_stat (env : env) (stat : stat) (k : unit -> unit) (co : coroutine) : unit =
  match stat with
    | Nop -> k ()
    | Seq(s1, s2) -> interp_stat env s1 (fun () -> interp_stat env s2 k co) co
    | FunctionCall fc -> interp_funcall env fc (fun v -> k ()) co
    | Assign(v,e) -> begin
                       match v with
                         | Name n -> interp_exp env e (fun v -> Value.set_ident env n v) co; k ()
                         | IndexTable(e1, e2) -> interp_exp env e1 (fun ve1 ->
                                                  interp_exp env e2 (fun ve2 ->
                                                    interp_exp env e (fun ve ->
                                                      k (Hashtbl.replace (Value.as_table ve1) (Value.as_table_key ve2) ve)
                                                    ) co
                                                  ) co
                                                ) co
                     end
    | If(e,s1,s2) -> interp_exp env e (fun ve -> 
                  if (Value.as_bool ve) then
                    interp_stat env s1 (fun () -> k ()) co
                  else
                    interp_stat env s2 (fun () -> k ()) co
                  ) co
    | WhileDoEnd(e,s) ->let rec aux () =
                          interp_exp env e (fun ve ->
                          if (Value.as_bool ve) then
                            interp_stat env s (fun () -> aux ()) co
                          else
                            k ()
                          ) co
                        in
                        aux ()


and print_args (vargs : value list) : unit =
  match vargs with
    | [] -> print_newline()
    | [a] -> print_string (Value.to_string a); print_args []
    | h::t -> print_string ( (Value.to_string h) ^ "\t" ); print_args t

and interp_funcall (env : env) (fc : functioncall) (k: value -> unit) (co : coroutine) : unit =
  interp_exp env (fst fc) (fun ve ->
    let rec aux le vargs = match le with
      | h::t -> interp_exp env h (fun v -> aux t (v::vargs)) co
      | [] -> let rvargs = List.rev vargs in
              let rec runfun f nvargs nk nco = match f with
                | Value.Print -> print_args rvargs; k Value.Nil
                | Value.Closure(nl, menv, b) -> let rec set_args nl vl = match nl, vl with
                                                  | [],_ -> ()
                                                  | hn::tn, [] -> Value.set_ident menv hn Value.Nil; set_args tn []
                                                  | hn::tn, hv::tv -> Value.set_ident menv hn hv; set_args tn tv
                                                in
                                        set_args nl rvargs;
                                        interp_block menv b (fun v -> k v) nco
                | Value.CoroutCreate -> let rec cc = Value.{stat = Suspended (fun va ->
                                            runfun (Value.as_function (List.hd rvargs)) [va] (fun v ->
                                              begin match cc.stat with
                                                | Running k1 -> cc.stat <- Dead; k1 v
                                                | _ -> failwith "Coroutine is not running"
                                              end
                                          ) cc
                                        )}
                                        in k (Value.Coroutine cc)
                |  Value.CoroutResume -> let cc = Value.as_coroutine (List.hd rvargs) in
                                         begin match cc.stat with
                                          | Suspended k1 -> cc.stat <- Running k;
                                                            begin match rvargs with
                                                              | h::t -> k1 h
                                                              | [] -> k1 Value.Nil
                                                            end
                                          | _ -> failwith "Can't resume coroutine"
                                         end
                | Value.CoroutYield -> begin match co.stat with
                                            | Running k1 -> co.stat <- Suspended k;
                                                            begin match rvargs with
                                                              | [] -> k1 Value.Nil
                                                              | h::t -> k1 h
                                                            end
                                            | _ -> failwith "Can't yield : coroutine is not running"
                                          end
                | Value.CoroutStatus -> let cc = Value.as_coroutine (List.hd rvargs) in
                                        begin match cc.stat with 
                                          | Running _ -> k (String "running")
                                          | Suspended _ -> k (String "suspended")
                                          | Dead -> k (String "dead")
                                        end
              in
              runfun (Value.as_function ve) vargs k co
    in
    aux (snd fc) []
  ) co


and interp_exp (env : env) (e : exp) (k: value -> unit) (co : coroutine) : unit =
  match e with
    | Nil -> k Nil
    | False -> k (Bool false)
    | True -> k (Bool true)
    | Integer i -> k (Int i)
    | Float f -> k (Float f)
    | LiteralString s -> k (String s)
    | Var v -> begin
                match v with
                  | Name n -> k (Value.lookup_ident env n)
                  | IndexTable(e1, e2) -> interp_exp env e1 (fun ve1 ->
                                            interp_exp env e2 (fun ve2 ->
                                              let v = try
                                                Hashtbl.find (Value.as_table ve1) (Value.as_table_key ve2)
                                              with Not_found -> Value.Nil in
                                              k v
                                            ) co
                                          ) co
              end
    | UnOp(o, e) -> interp_exp env e (fun ve ->
                      begin
                        match o with
                          | Not -> k (Bool ( not (Value.as_bool ve) ))
                          | UnaryMinus -> k (Value.neg ve)
                      end
                    ) co
    | BinOp(o, e1, e2) -> interp_exp env e1 (fun ve1 ->
                            begin
                                match o with
                                  | LogicalAnd -> begin match Value.as_bool ve1 with
                                                    | false -> k ve1
                                                    | true -> interp_exp env e2 (fun ve2 -> k ve2) co
                                                  end
                                  | LogicalOr -> begin match Value.as_bool ve1 with
                                                    | true -> k ve1
                                                    | false -> interp_exp env e2 (fun ve2 -> k ve2) co
                                                  end
                                  | _ ->  interp_exp env e2 (fun ve2 ->
                                          k (begin
                                            match o with
                                              | Equality -> Bool (Value.equal ve1 ve2)
                                              | Addition -> Value.add ve1 ve2
                                              | Subtraction -> Value.sub ve1 ve2
                                              | Multiplication -> Value.mul ve1 ve2
                                              | Inequality -> Bool (not (Value.equal ve1 ve2))
                                              | Less -> Bool (Value.lt ve1 ve2)
                                              | LessEq -> Bool (Value.le ve1 ve2)
                                              | Greater -> Bool (Value.lt ve2 ve1)
                                              | GreaterEq -> Bool (Value.le ve2 ve1)
                                          end)
                                        ) co
                            end) co
    | FunctionCallE fc -> interp_funcall env fc (fun v -> k v) co
    | FunctionDef fb -> let new_scope = create_scope (fst fb) (List.map (fun x -> Value.Nil) (fst fb)) in
                        let nenv = Value.{ globals = env.globals; locals = new_scope::env.locals } in
                        k (Value.Function ( Closure(fst fb, nenv, snd fb)  ))
    | Table l -> let ht = Hashtbl.create (List.length l) in
                 let rec aux li = match li with
                  | (e1, e2)::t -> interp_exp env e1 (fun ve1 ->
                                    interp_exp env e2 (fun ve2 ->
                                      Hashtbl.add ht (Value.as_table_key ve1) ve2;
                                      aux t
                                    ) co
                                   ) co
                  | [] -> k (Table ht)
                in
                aux l


let run ast =
  let coroutine : (Value.tkey, value) Hashtbl.t = Hashtbl.create 4 in
  Hashtbl.add coroutine (KString "create") (Value.Function CoroutCreate);
  Hashtbl.add coroutine (KString "yield")  (Value.Function CoroutYield);
  Hashtbl.add coroutine (KString "mini_resume") (Value.Function CoroutResume);
  Hashtbl.add coroutine (KString "status") (Value.Function CoroutStatus);
  let globals : (string, value) Hashtbl.t = Hashtbl.create 47 in
  Hashtbl.add globals "print" (Function Print);
  Hashtbl.add globals "coroutine" (Table coroutine);
  let env = Value.{ globals; locals = [] } in
  interp_block env ast (fun x -> ()) Value.{stat = Running (fun v -> failwith "not a coroutine")}
