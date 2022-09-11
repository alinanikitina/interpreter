open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e =
    match e with
    | Value (v) -> v
    | ID (id) -> (lookup env id)
    | Fun (name, expr) -> (Closure (env, name, expr))
    | Not (expr) -> (match expr with
                    | Value (v) -> (match v with
                                   | Bool (b) -> if b = true then (Bool (false)) else (Bool (true))
                                   | _ -> raise (TypeError "expected boolean"))
                    | _ -> raise (TypeError "expected boolean"))
    | Binop (op, e1, e2) -> let v1 = eval_expr env e1 in let v2 = eval_expr env e2 in
                            (match op, v1, v2 with
                            | (Add, (Int (i1)), (Int (i2))) -> Int ((i1 + i2))
                            | (Sub, (Int (i1)), (Int (i2))) -> Int ((i1 - i2))
                            | (Mult, (Int (i1)), (Int (i2))) -> Int ((i1 * i2))
                            | (Div, (Int (i1)), (Int (i2))) -> if i2 = 0 then raise (DivByZeroError) else Int ((i1 / i2))
                            | (Greater, (Int (i1)), (Int (i2))) -> Bool ((if i1 > i2 then true else false))
                            | (Less, (Int (i1)), (Int (i2))) -> Bool ((if i1 < i2 then true else false))
                            | (GreaterEqual, (Int (i1)), (Int (i2))) -> Bool ((if i1 >= i2 then true else false))
                            | (LessEqual, (Int (i1)), (Int (i2))) -> Bool ((if i1 <= i2 then true else false))
                            | (Concat, (String (s1)), (String (s2))) -> String ((s1 ^ s2))
                            | (Equal, (Int (i1)), (Int (i2))) -> Bool ((if i1 = i2 then true else false))
                            | (Equal, (Bool (b1)), (Bool (b2))) -> Bool ((if b1 = b2 then true else false))
                            | (Equal, (String (s1)), (String (s2))) -> Bool ((if s1 = s2 then true else false))
                            | (NotEqual, (Int (i1)), (Int (i2))) -> Bool ((if not (i1 = i2) then true else false))
                            | (NotEqual, (Bool (b1)), (Bool (b2))) -> Bool ((if not (b1 = b2) then true else false))
                            | (NotEqual, (String (s1)), (String (s2))) -> Bool ((if not (s1 = s2) then true else false))
                            | (Or, (Bool (b1)), (Bool (b2))) -> Bool ((if b1 || b2 then true else false))
                            | (And, (Bool (b1)), (Bool (b2))) -> Bool ((if b1 && b2 then true else false))
                            | (_, _, _) -> raise (TypeError "invalid type(s)"))
    | If (e1, e2, e3) -> (match eval_expr env e1 with
                         | Bool (true) -> eval_expr env e2
                         | Bool (false) -> eval_expr env e3
                         | _ -> raise (TypeError "first expression did not evaluate to boolean"))
    | FunctionCall (e1, e2) -> (match (eval_expr env e1) with
                               | Closure (a, x, e) -> let v = (eval_expr env e2) in (eval_expr (extend a x v) e)
                               | _ -> raise (TypeError "expected closure"))
    | Let (id, b, e1, e2) -> if b = true then let temp_e = (extend_tmp env id) in let v = (eval_expr temp_e e1) in (update temp_e id v); (eval_expr temp_e e2)
                             else let v = (eval_expr env e1) in (eval_expr (extend env id v) e2)

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = match m with
                       | Def (var, expr) -> let temp_e = (extend_tmp env var) in let v = (eval_expr temp_e expr) in (update temp_e var v); (temp_e, Some (v))
                       | Expr (expr) -> let v = (eval_expr env expr) in (env, Some (v))
                       | NoOp -> (env, None)
