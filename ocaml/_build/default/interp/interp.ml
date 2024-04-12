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
  (* env.locals <- ()::env.locals *)
  interp_stat env (blk.body);
  interp_exp env (blk.ret)
  

and interp_stat (env : env) (stat : stat) : unit =
  match stat with
    | Nop -> ()
    | Seq(s1, s2) -> interp_stat env s1; interp_stat env s2
    | FunctionCall f -> ignore (interp_funcall env f)
    | _ -> ()


and print_args (args : value list) : unit =
  List.iter (fun t -> print_string ( (Value.to_string t) ^ "\t") ) args

(* functioncall = exp * args *)
(* and args = exp list *)

and interp_funcall (env : env) (fc : functioncall) : value =
  let e = interp_exp env (fst fc) in
  let args = List.map (fun a -> interp_exp env a) (snd fc) in
  match e with
    | String s -> let f = Hashtbl.find env.globals s in
                  begin
                    match f with
                      | Value.Function Print -> print_args args; Nil
                      | _ -> Nil
                  end
    | _ -> Nil


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

(* and var =
  | Name       of name (* nom de variable *)
  | IndexTable of exp * exp (* index dans une table: e[e] *) *)

and interp_exp (env : env) (e : exp) : value =
  match e with
    | Var v -> begin
                match v with
                  | Name n -> String n
                  | _ -> Nil
              end
    | Nil -> Nil
    | False -> Bool false
    | True -> Bool true
    | Integer i -> Int i
    | LiteralString s -> String s
    | _ -> Nil

let run ast =
  let globals = Hashtbl.create 47 in
  Hashtbl.add globals "print" (Value.Function Print);
  let env = Value.{ globals; locals = [] } in
  ignore (interp_block env ast)
