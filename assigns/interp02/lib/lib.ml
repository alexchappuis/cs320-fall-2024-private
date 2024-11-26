open Utils

let parse = My_parser.parse

let rec desugar prog =
  match prog with
  | [] -> Unit
  | stmt :: rest -> (
      match stmt with
      | TopLet (x, ty, body) ->
          Let (x, ty, body, desugar rest)
      | TopLetRec (f, x, ty_arg, ty_out, body) ->
          LetRec (f, x, ty_arg, ty_out, body, desugar rest)
    )

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
    | Add (e1, e2) | Sub (e1, e2) | Mul (e1, e2) ->
        if infer e1 = Some IntTy && infer e2 = Some IntTy then Some IntTy
        else None
    | Eq (e1, e2) ->
        if infer e1 = Some IntTy && infer e2 = Some IntTy then Some BoolTy
        else None
    | If (e1, e2, e3) -> (
        match infer e1, infer e2, infer e3 with
        | Some BoolTy, Some t2, Some t3 when t2 = t3 -> Some t2
        | _ -> None
      )
    | Let (x, ty, e1, e2) -> (
        match infer e1 with
        | Some t1 when t1 = ty -> type_of ((x, ty) :: ctxt) e2
        | _ -> None
      )
    | LetRec (f, x, ty_arg, ty_out, e1, e2) -> (
        let ctxt' = (f, FunTy (ty_arg, ty_out)) :: (x, ty_arg) :: ctxt in
        match type_of ctxt' e1 with
        | Some ty when ty = ty_out -> type_of ((f, FunTy (ty_arg, ty_out)) :: ctxt) e2
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
        | None -> raise (Failure "Unbound variable")
      )
    | Num n -> VNum n
    | Fun (x, _, body) -> VClos (x, body, env, None)
    | Add (e1, e2) ->
        (match eval_expr e1, eval_expr e2 with
        | VNum n1, VNum n2 -> VNum (n1 + n2)
        | _ -> raise (Failure "Invalid"))
    | Sub (e1, e2) ->
        (match eval_expr e1, eval_expr e2 with
        | VNum n1, VNum n2 -> VNum (n1 - n2)
        | _ -> raise (Failure "Invalid"))
    | Mul (e1, e2) ->
        (match eval_expr e1, eval_expr e2 with
        | VNum n1, VNum n2 -> VNum (n1 * n2)
        | _ -> raise (Failure "Invalid"))
    | Eq (e1, e2) ->
        (match eval_expr e1, eval_expr e2 with
        | VNum n1, VNum n2 -> VBool (n1 = n2)
        | _ -> raise (Failure "Invalid"))
    | If (e1, e2, e3) ->
        (match eval_expr e1 with
        | VBool true -> eval_expr e2
        | VBool false -> eval_expr e3
        | _ -> raise (Failure "Invalid"))
    | Let (x, _, e1, e2) ->
        let v = eval_expr e1 in
        eval (Env.add x v env) e2
    | LetRec (f, x, _, _, e1, e2) ->
        let rec_env = Env.add f (VClos (x, e1, env, Some f)) env in
        eval rec_env e2
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
          | AssertFail -> Error "Invalid"
          | DivByZero -> Error "Invalid"
          | Failure msg -> Error msg)
      | None -> Error "Invalidd"
    )
  | None -> Error "Parsing failed"