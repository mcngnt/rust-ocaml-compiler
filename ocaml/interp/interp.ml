open Luaparser.Ast
type value = Value.t
type env = Value.env

let create_scope (names: string list) (values: value list) : (name, value) Hashtbl.t =
  assert false


(* type block = {
  locals: name list;
  body: stat;
  ret: exp
} *)

let rec interp_block (env : env) (blk : block) : value =
  let hloc = Hashtbl.create (List.length blk.locals) in
  List.iter (fun n -> Hashtbl.add hloc n Value.Nil) blk.locals;
  let nenv = Value.{ globals = env.globals; locals = hloc::env.locals } in
  interp_stat nenv (blk.body);
  interp_exp nenv (blk.ret)

(*   (* le statement vide qui ne fait rien *)
  | Nop
  (* séquence de deux statements *)
  | Seq              of stat * stat
  (* Assignation: "x = e" ou "e[e] = e" *)
  | Assign           of var * exp
  (* Appel de fonction: "e(e, ..., e)" *)
  | FunctionCall     of functioncall
  (* Boucle while: "while e do ... end" *)
  | WhileDoEnd       of exp * stat
  (* If-then-else: "if e then ... else ... end" *)
  | If               of exp * stat * stat

   *) 

and interp_stat (env : env) (stat : stat) : unit =
  match stat with
    | Nop -> ()
    | Seq(s1, s2) -> interp_stat env s1; interp_stat env s2
    | FunctionCall fc -> let _ = interp_funcall env fc in ()
    | Assign(v,e) -> begin
                       match v with
                         | Name n -> Value.set_ident env n (interp_exp env e)
                         | IndexTable(e1, e2) -> let t = Value.as_table (interp_exp env e1) in
                                                 let k = Value.as_table_key (interp_exp env e2) in
                                                  try
                                                    Hashtbl.replace t k (interp_exp env e)
                                                  with Not_found -> ()
                     end
    | If(e,s1,s2) -> let ve = interp_exp env e in
                  if (Value.as_bool ve) then
                    interp_stat env s1
                  else
                    interp_stat env s2
    | WhileDoEnd(e,s) -> let ve = interp_exp env e in
                        if (Value.as_bool ve) then begin
                          interp_stat env s;
                          interp_stat env (WhileDoEnd(e,s))
                        end
    | _ -> ()


and print_args (vargs : value list) : unit =
  match vargs with
    | [] -> print_newline()
    | [a] -> print_string (Value.to_string a); print_args []
    | h::t -> print_string ( (Value.to_string h) ^ "\t" ); print_args t



(* functioncall = exp * args *)
(* and args = exp list *)

(* and func =
  (* fermeture: noms des arguments, environnement et code *)
  | Closure of name list * env * block
  | Print (* la fonction primitive 'print' *) *)

and interp_funcall (env : env) (fc : functioncall) : value =
  let ve = interp_exp env (fst fc) in
  let vargs = List.map (fun a -> interp_exp env a) (snd fc) in
  match ve with
    | Function f -> begin
                    match f with
                      | Print -> print_args vargs; Nil
                      | Closure(nl, menv, b) -> 
                        let rec aux nl vl = match nl, vl with
                          | [],_ -> ()
                          | hn::tn, [] -> Value.set_ident menv hn Value.Nil; aux tn []
                          | hn::tn, hv::tv -> Value.set_ident menv hn hv; aux tn tv
                        in
                        aux nl vargs;
                        interp_block menv b
                    end
    | _ -> failwith "Object not callable."


(* type t =
  | Nil
  | Bool of bool
  | Int of Int64.t
  | Float of float
  | String of string
  | Function of func
  | Table of (tkey, t) Hashtbl.t
 *)
(* and exp =
  | Nil
  | False
  | True
  | Integer        of Int64.t
  | Float          of float
  | LiteralString  of string (* chaîne de caractères: de la forme "abc" *)
  | Var            of var (* nom de variable *)
  | FunctionCallE  of functioncall (* appel de fonction "e(e, ..., e)" *)
  | FunctionDef    of funcbody (* fonction anonyme: function (x, y, ...) ... end *)
  | BinOp          of binop * exp * exp (* e op e *)
  | UnOp           of unop * exp (* - e *)
  | Table          of (exp * exp) list *)


(* and funcbody = name list * block *)

(* and var =
  | Name       of name (* nom de variable *)
  | IndexTable of exp * exp (* index dans une table: e[e] *) *)

(* 

and binop =
  (* arithmetic operators *)
  | Addition (* + *)
  | Subtraction (* - *)
  | Multiplication (* * *)
  (* relational operators *)
  | Equality (* == *)
  | Inequality (* ~= *)
  | Less (* < *)
  | Greater (* > *)
  | LessEq (* <= *)
  | GreaterEq (* >= *)
  (* logical operators *)
  | LogicalAnd (* and *)
  | LogicalOr (* or *) *)


and interp_exp (env : env) (e : exp) : value =
  match e with
    | Var v -> begin
                match v with
                  | Name n -> Value.lookup_ident env n
                  | IndexTable(e1, e2) -> let t = Value.as_table (interp_exp env e1) in
                                          let k = Value.as_table_key (interp_exp env e2) in
                                          try
                                            Hashtbl.find t k
                                          with Not_found -> Value.Nil
              end
    | UnOp(o, e) -> let ve = interp_exp env e in 
                      begin
                        match o with
                          | Not -> Bool ( not (Value.as_bool ve) )
                          | UnaryMinus -> Value.neg ve
                      end
    | BinOp(o, e1, e2) -> let ve1 = interp_exp env e1 in
                            begin
                                match o with
                                  | LogicalAnd -> begin match Value.as_bool ve1 with
                                                    | false -> ve1
                                                    | true -> interp_exp env e2
                                                  end
                                  | LogicalOr -> begin match Value.as_bool ve1 with
                                                    | true -> ve1
                                                    | false -> interp_exp env e2
                                                  end
                                  | _ ->  let ve2 = interp_exp env e2 in
                                          begin
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
                                              | _ -> Nil
                                          end
                            end
    | Nil -> Nil
    | False -> Bool false
    | True -> Bool true
    | Integer i -> Int i
    | Float f -> Float f
    | LiteralString s -> String s
    | FunctionCallE fc -> interp_funcall env fc
    | FunctionDef fb -> Value.Function ( Closure(fst fb, env, snd fb)  )
    | Table l -> let ht = Hashtbl.create (List.length l) in
                 List.iter (fun (e1, e2) -> let ve1 = interp_exp env e1 in
                                            let ve2 = interp_exp env e2 in
                                            let key = match ve1 with
                                              | Int i -> Value.KInt i
                                              | String s -> Value.KString s
                                              | _ -> failwith "Wrong key type in table"
                                            in
                                            Hashtbl.add ht key ve2 ) l;
                 Table ht
    | _ -> Nil

let run ast =
  let globals = Hashtbl.create 47 in
  Hashtbl.add globals "print" (Value.Function Print);
  let env = Value.{ globals; locals = [] } in
  ignore (interp_block env ast)
