open Luaparser.Ast
type value = Value.t
type coroutine = Value.coroutine
type env = Value.env

let create_scope (names: string list) (values: value list) : (name, value) Hashtbl.t =
  let ht = Hashtbl.create (List.length names) in
  List.iter2 (fun n v -> Hashtbl.add ht n v) names values;
  ht

let rec interp_block (env : env) (blk : block) (k : value -> unit) (co : value -> unit) : unit =
  let new_scope = create_scope blk.locals (List.map (fun x -> Value.Nil) blk.locals) in
  let nenv = Value.{ globals = env.globals; locals = new_scope::env.locals } in
  interp_stat nenv (blk.body) (fun v -> interp_exp nenv (blk.ret) k co) co

and interp_stat (env : env) (stat : stat) (k : unit -> unit) (co : value -> unit) : unit =
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

and interp_funcall (env : env) (fc : functioncall) (k: value -> unit) (co : value -> unit) : unit =
  interp_exp env (fst fc) (fun ve ->
    let rec aux le vargs = match le with
      | h::t -> interp_exp env h (fun v -> aux t (v::vargs)) co
      | [] -> match ve with
                | Function f -> begin
                                match f with
                                  | Print -> print_args (List.rev vargs); k Value.Nil
                                  | Closure(nl, menv, b) -> 
                                      let rec aux nl vl = match nl, vl with
                                        | [],_ -> ()
                                        | hn::tn, [] -> Value.set_ident menv hn Value.Nil; aux tn []
                                        | hn::tn, hv::tv -> Value.set_ident menv hn hv; aux tn tv
                                      in
                                      aux nl (List.rev vargs);
                                      interp_block menv b (fun v -> k v) co
                                  (* | CoroutCreate -> Value.coroutine{Supsended c} *)
                                  | _ -> k Value.Nil
                                end
                | _ -> failwith "Object is not callable."
    in
    aux (snd fc) []
  ) co


and interp_exp (env : env) (e : exp) (k: value -> unit) (co : value -> unit) : unit =
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
                 (* List.iter (fun (e1, e2) -> let ve1 = interp_exp env e1 in
                                            let ve2 = interp_exp env e2 in
                                            let key = Value.as_table_key ve1 in
                                            Hashtbl.add ht key ve2 ) l;
                 Table ht *)
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
  ignore (interp_block env ast (fun x -> ()) (fun v -> ()))
