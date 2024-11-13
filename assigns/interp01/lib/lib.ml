
open Utils 

let parse = My_parser.parse


let convert_value_to_expr = function
  | VNum number -> Num number
  | VBool bool_val -> if bool_val then True else False
  | VUnit -> Unit
  | VFun (param, func_body) -> Fun (param, func_body)

let substitute_var a b =
  let rec internal_replace = function
    | Var var_name -> if var_name = b then Var a else Var var_name
    | App (left, right) -> App (internal_replace left, internal_replace right)
    | Fun (var_name, expr) -> if var_name = b then Fun (var_name, expr) else Fun (var_name, internal_replace expr)
    | Let (var_name, left_expr, right_expr) -> 
        if var_name = b then Let (var_name, internal_replace left_expr, right_expr) 
        else Let (var_name, internal_replace left_expr, internal_replace right_expr)
    | If (condition, then_expr, else_expr) -> If (internal_replace condition, internal_replace then_expr, internal_replace else_expr)
    | Bop (operator, left_expr, right_expr) -> Bop (operator, internal_replace left_expr, internal_replace right_expr)
    | Num n -> Num n
    | True -> True
    | False -> False
    | Unit -> Unit
  in
  internal_replace

let rec subst (value : value) (var_name : string) (expression : expr) : expr =
  match expression with
  | Num _ | True | False | Unit -> expression
  | Var other_var -> if other_var = var_name then convert_value_to_expr value else expression
  | If (condition, then_branch, else_branch) -> If (subst value var_name condition, subst value var_name then_branch, subst value var_name else_branch)
  | Let (other_var, init_expr, body_expr) ->
      if other_var = var_name then Let (other_var, subst value var_name init_expr, body_expr)
      else
        let new_var = gensym () in
        Let (new_var, subst value var_name init_expr, subst value var_name (substitute_var new_var other_var body_expr))
  | Fun (other_var, func_body) -> if other_var = var_name then expression else
      let new_var = gensym () in
      Fun (new_var, subst value var_name (substitute_var new_var other_var func_body))
  | App (func_expr, arg_expr) -> App (subst value var_name func_expr, subst value var_name arg_expr)
  | Bop (operator, left_expr, right_expr) -> Bop (operator, subst value var_name left_expr, subst value var_name right_expr)

let evaluate_binary_operation operator val1 val2 =
  match operator, val1, val2 with
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
  | _ -> Error (InvalidArgs operator)

let rec eval (expr : expr) : (value, error) result =
  match expr with
  | Num num -> Ok (VNum num)
  | True -> Ok (VBool true)
  | False -> Ok (VBool false)
  | Unit -> Ok VUnit
  | Var var_name -> Error (UnknownVar var_name)
  | Bop (And, expr1, expr2) -> (
      match eval expr1 with
      | Ok (VBool false) -> Ok (VBool false)
      | Ok (VBool true) -> eval expr2
      | _ -> Error InvalidIfCond)
  | Bop (Or, expr1, expr2) -> (
      match eval expr1 with
      | Ok (VBool true) -> Ok (VBool true)
      | Ok (VBool false) -> eval expr2
      | _ -> Error InvalidIfCond)
  | If (condition, then_branch, else_branch) -> (
      match eval condition with
      | Ok (VBool true) -> eval then_branch
      | Ok (VBool false) -> eval else_branch
      | _ -> Error InvalidIfCond)
  | Let (var_name, expr1, expr2) -> (
      match eval expr1 with
      | Ok val_result -> eval (subst val_result var_name expr2)
      | Error error_val -> Error error_val)
  | Fun (param, func_body) -> Ok (VFun (param, func_body))
  | App (func_expr, arg_expr) -> (
      match eval func_expr with
      | Ok (VFun (param, func_body)) -> (
          match eval arg_expr with
          | Ok arg_value -> eval (subst arg_value param func_body)
          | Error error_val -> Error error_val)
      | Ok _ -> Error InvalidApp
      | Error error_val -> Error error_val)
  | Bop (operator, left_expr, right_expr) -> (
      match eval left_expr with
      | Ok left_val -> (
          match eval right_expr with
          | Ok right_val -> evaluate_binary_operation operator left_val right_val
          | Error error_val -> Error error_val)
      | Error error_val -> Error error_val)
  
let interp (code_input : string) : (value, error) result =
  match parse code_input with
  | Some program -> eval program
  | None -> Error ParseFail