open Utils

let parse = My_parser.parse

let rec desugar prog =
  match prog with
  | [] -> Unit  
  | l :: ls -> (
      match l with
      | TopLet (name, ty, value) ->
          Let (name, ty, desugar_sfexpr value, desugar ls)
      
      | TopLetRec (name, arg, ty_arg, ty_out, value) ->
          LetRec (name, arg, ty_arg, ty_out, desugar_sfexpr value, desugar ls)
    )

and desugar_sfexpr expr =
  match expr with
  | SUnit -> Unit
  | STrue -> True
  | SFalse -> False
  | SNum n -> Num n
  | SVar v -> Var v
  | SFun { arg; args; body } -> 
      Fun (arg, UnitTy, desugar_sfexpr body) 
      App (desugar_sfexpr e1, desugar_sfexpr e2)
  | SLet { is_rec; name; args; ty; value; body } -> 
      Let (name, ty, desugar_sfexpr value, desugar_sfexpr body)
  | SIf (cond, then_branch, else_branch) -> 
      If (desugar_sfexpr cond, desugar_sfexpr then_branch, desugar_sfexpr else_branch)
  | SBop (op, e1, e2) -> 
      Bop (op, desugar_sfexpr e1, desugar_sfexpr e2)
  | SAssert e -> 
      Assert (desugar_sfexpr e)

let rec type_of ctxt expr =
  let rec infer = function
    | Unit -> Some UnitTy
    | True -> Some BoolTy
    | False -> Some BoolTy
    | Var x -> List.assoc_opt x ctxt
    | Num _ -> Some IntTy
    | Fun (x, ty, body) -> (
        match type_of ((x, ty) :: ctxt) body with
        | Some ty_out -> Some (FunTy (ty, ty_out))
        | None -> None
      )
    | Bop (op, e1, e2) -> (
        match op with
        | Add | Sub | Mul | Div | Mod ->
            if infer e1 = Some IntTy && infer e2 = Some IntTy then Some IntTy
            else None
        | Eq ->
            if infer e1 = Some IntTy && infer e2 = Some IntTy then Some BoolTy
            else None
        | _ -> None
      )
    | If (e1, e2, e3) -> (
        match infer e1, infer e2, infer e3 with
        | Some BoolTy, Some t2, Some t3 when t2 = t3 -> Some t2
        | _ -> None
      )
    | Let { is_rec; name; ty; value; body } -> (
        match infer value with
        | Some v_ty when v_ty = ty -> type_of ((name, ty) :: ctxt) body
        | _ -> None
      )
    | Assert e -> (
        match infer e with
        | Some BoolTy -> Some BoolTy
        | _ -> None
      )
    | App (e1, e2) -> (
        match infer e1, infer e2 with
        | Some (FunTy (ty_arg, ty_out)), Some t2 when ty_arg = t2 -> Some ty_out
        | _ -> None
      )
  in
  infer expr

exception AssertFail
exception DivByZero

let rec eval env expr =
  let rec eval_expr = function
    | Unit -> VUnit
    | True -> VBool true
    | False -> VBool false
    | Var x -> (
        match Env.find_opt x env with
        | Some v -> v
        | None -> raise (Failure "Invalid")
      )
    | Num n -> VNum n
    | Fun (x, _, body) -> VClos (x, body, env, None)
    | Bop (op, e1, e2) -> (
        match eval_expr e1, eval_expr e2 with
        | VNum n1, VNum n2 -> (
            match op with
            | Add -> VNum (n1 + n2)
            | Sub -> VNum (n1 - n2)
            | Mul -> VNum (n1 * n2)
            | Div -> if n2 = 0 then raise DivByZero else VNum (n1 / n2)
            | Mod -> VNum (n1 mod n2)
            | _ -> raise (Failure "Invalid")
          )
        | _ -> raise (Failure "Invalid operands")
      )
    | Eq (e1, e2) -> (
        match eval_expr e1, eval_expr e2 with
        | VNum n1, VNum n2 -> VBool (n1 = n2)
        | _ -> raise (Failure "Invalid")
      )
    | If (e1, e2, e3) ->
        (match eval_expr e1 with
        | VBool true -> eval_expr e2
        | VBool false -> eval_expr e3
        | _ -> raise (Failure "Invalid"))
    | Let { is_rec; name; ty; value; body } ->
        let v = eval_expr value in
        eval (Env.add name v env) body
    | Assert e -> (
        match eval_expr e with
        | VBool true -> VUnit
        | VBool false -> raise AssertFail
        | _ -> raise (Failure "Invalid")
      )
    | App (e1, e2) ->
        (match eval_expr e1 with
        | VClos (x, body, closure_env, None) ->
            let v = eval_expr e2 in
            eval (Env.add x v closure_env) body
        | VClos (x, body, closure_env, Some f) ->
            let v = eval_expr e2 in
            let new_env =
              Env.add f (VClos (x, body, closure_env, Some f)) closure_env
            in
            eval (Env.add x v new_env) body
        | _ -> raise (Failure "Invalid"))
  in
  eval_expr expr

let interp str =
  match My_parser.parse str with
  | Some prog -> (
      let expr = desugar prog in
      match type_of [] expr with
      | Some _ -> (
          try Ok (eval Env.empty expr)
          with
          | AssertFail -> Error "failed"
          | DivByZero -> Error "Invalid"
          | Failure msg -> Error msg)
      | None -> Error "Type error"
    )
  | None -> Error "Parsing failed"
