
open Utils

let parse = My_parser.parse

let convert_value_to_expr = function
  | VNum n -> Num n
  | VBool b -> if b then True else False
  | VUnit -> Unit
  | VFun (param, body) -> Fun (param, body)

let substitute_var a b =
  let rec internal_replace = function
    | Var v -> if v = b then Var a else Var v
    | App (l, r) -> App (internal_replace l, internal_replace r)
    | Fun (v, body) -> if v = b then Fun (v, body) else Fun (v, internal_replace body)
    | Let (v, l, r) -> if v = b then Let (v, internal_replace l, r) else Let (v, internal_replace l, internal_replace r)
    | If (cond, then_expr, else_expr) -> If (internal_replace cond, internal_replace then_expr, internal_replace else_expr)
    | Bop (op, l, r) -> Bop (op, internal_replace l, internal_replace r)
    | Num n -> Num n
    | True -> True
    | False -> False
    | Unit -> Unit
  in internal_replace

  let rec subst value var expr =
    match expr with
    | Num _ | True | False | Unit -> expr
    | Var v -> if v = var then convert_value_to_expr value else expr
    | If (cond, then_expr, else_expr) ->
        If (subst value var cond, subst value var then_expr, subst value var else_expr)
    | Let (v, init_expr, body_expr) ->
        if v = var then Let (v, subst value var init_expr, body_expr)
        else Let (v, subst value var init_expr, subst value var body_expr)
    | Fun (v, body) ->
        if v = var then expr
        else Fun (v, subst value var body)
    | App (f_expr, arg_expr) -> App (subst value var f_expr, subst value var arg_expr)
    | Bop (op, left_expr, right_expr) ->
        Bop (op, subst value var left_expr, subst value var right_expr)
  

let evaluate_binary_operation op v1 v2 =
  match op, v1, v2 with
  | Add, VNum n1, VNum n2 -> Ok (VNum (n1 + n2))
  | Sub, VNum n1, VNum n2 -> Ok (VNum (n1 - n2))
  | Mul, VNum n1, VNum n2 -> Ok (VNum (n1 * n2))
  | Div, VNum n1, VNum n2 -> if n2 = 0 then Error DivByZero else Ok (VNum (n1 / n2))
  | Mod, VNum n1, VNum n2 -> if n2 = 0 then Error DivByZero else Ok (VNum (n1 mod n2))
  | Lt, VNum n1, VNum n2 -> Ok (VBool (n1 < n2))
  | Lte, VNum n1, VNum n2 -> Ok (VBool (n1 <= n2))
  | Gt, VNum n1, VNum n2 -> Ok (VBool (n1 > n2))
  | Gte, VNum n1, VNum n2 -> Ok (VBool (n1 >= n2))
  | Eq, VNum n1, VNum n2 -> Ok (VBool (n1 = n2))
  | Neq, VNum n1, VNum n2 -> Ok (VBool (n1 <> n2))
  | And, VBool b1, VBool b2 -> Ok (VBool (b1 && b2))
  | Or, VBool b1, VBool b2 -> Ok (VBool (b1 || b2))
  | _ -> Error (InvalidArgs op)

let rec eval expr =
  match expr with
  | Num n -> Ok (VNum n)
  | True -> Ok (VBool true)
  | False -> Ok (VBool false)
  | Unit -> Ok VUnit
  | Var v -> Error (UnknownVar v)
  | Bop (And, e1, e2) -> 
      (match eval e1 with
       | Ok (VBool false) -> Ok (VBool false)
       | Ok (VBool true) -> eval e2
       | _ -> Error InvalidIfCond)
  | Bop (Or, e1, e2) -> 
      (match eval e1 with
       | Ok (VBool true) -> Ok (VBool true)
       | Ok (VBool false) -> eval e2
       | _ -> Error InvalidIfCond)
  | If (cond, then_expr, else_expr) -> 
      (match eval cond with
       | Ok (VBool true) -> eval then_expr
       | Ok (VBool false) -> eval else_expr
       | _ -> Error InvalidIfCond)
  | Let (v, e1, e2) -> 
      (match eval e1 with
       | Ok v1 -> eval (subst v1 v e2)
       | Error e -> Error e)
  | Fun (param, body) -> Ok (VFun (param, body))
  | App (f, arg) -> 
      (match eval f with
       | Ok (VFun (param, body)) -> 
           (match eval arg with
            | Ok v -> eval (subst v param body)
            | Error e -> Error e)
       | Ok _ -> Error InvalidApp
       | Error e -> Error e)
  | Bop (op, e1, e2) -> 
      (match eval e1 with
       | Ok v1 -> (match eval e2 with
                  | Ok v2 -> evaluate_binary_operation op v1 v2
                  | Error e -> Error e)
       | Error e -> Error e)
       
let interp (code_input : string) : (value, error) result =
  match parse code_input with
  | Some program -> eval program
  | None -> Error ParseFail