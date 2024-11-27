open Utils
include My_parser

let parse = My_parser.parse

let rec desugar (prog : prog) : expr =
  match prog with
  | [] -> Unit
  | decl :: rest -> (
      let function_type =
        List.fold_right (fun (_, arg_ty) acc -> FunTy (arg_ty, acc)) decl.args decl.ty
      in
      let desugared_value =
        List.fold_right (fun (arg, arg_ty) acc -> Fun (arg, arg_ty, acc))
          decl.args
          (desugar_expr decl.value)
      in
      match decl.is_rec with
      | false ->
          Let { is_rec = false; name = decl.name; ty = function_type; value = desugared_value; body = desugar rest }
      | true ->
          Let { is_rec = true; name = decl.name; ty = function_type; value = desugared_value; body = desugar rest }
    )

and desugar_expr (expr : sfexpr) : expr =
  match expr with
  | SLet { is_rec; name; args; ty; value; body } -> (
      let function_type =
        List.fold_right (fun (_, arg_ty) acc -> FunTy (arg_ty, acc)) args ty
      in
      let desugared_value =
        List.fold_right (fun (arg, arg_ty) acc -> Fun (arg, arg_ty, acc))
          args
          (desugar_expr value)
      in
      match is_rec with
      | false -> Let { is_rec = false; name; ty = function_type; value = desugared_value; body = desugar_expr body }
      | true -> Let { is_rec = true; name; ty = function_type; value = desugared_value; body = desugar_expr body }
    )
  | SFun { arg; args; body } ->
      List.fold_right (fun (arg, arg_ty) acc -> Fun (arg, arg_ty, acc)) (arg :: args) (desugar_expr body)
  | SIf (cond, then_, else_) -> If (desugar_expr cond, desugar_expr then_, desugar_expr else_)
  | SApp (e1, e2) -> App (desugar_expr e1, desugar_expr e2)
  | SBop (op, e1, e2) -> Bop (op, desugar_expr e1, desugar_expr e2)
  | SAssert e -> Assert (desugar_expr e)
  | SUnit -> Unit
  | STrue -> True
  | SFalse -> False
  | SNum n -> Num n
  | SVar x -> Var x

exception AssertFail  
exception DivByZero

let type_of (e : expr) : (ty, error) result =
  let rec typecheck env expr =
    match expr with
    | Unit -> Ok UnitTy
    | True | False -> Ok BoolTy
    | Num _ -> Ok IntTy
    | Var x -> (
        match Env.find_opt x env with
        | Some ty -> Ok ty
        | None -> Error (UnknownVar x)
      )
    | If (cond, then_, else_) -> (
        match typecheck env cond with
        | Ok BoolTy -> (
            match typecheck env then_ with
            | Ok ty_then -> (
                match typecheck env else_ with
                | Ok ty_else when ty_then = ty_else -> Ok ty_then
                | Ok ty_else -> Error (IfTyErr (ty_then, ty_else))
                | Error e -> Error e
              )
            | Error e -> Error e
          )
        | Ok ty -> Error (IfCondTyErr ty)
        | Error e -> Error e
      )
    | Fun (arg, arg_ty, body) ->
        let extended_env = Env.add arg arg_ty env in
        (match typecheck extended_env body with
         | Ok body_ty -> Ok (FunTy (arg_ty, body_ty))
         | Error e -> Error e)
    | App (e1, e2) -> (
        match typecheck env e1 with
        | Ok (FunTy (arg_ty, ret_ty)) -> (
            match typecheck env e2 with
            | Ok actual_ty when arg_ty = actual_ty -> Ok ret_ty
            | Ok actual_ty -> Error (FunArgTyErr (arg_ty, actual_ty))
            | Error e -> Error e
          )
        | Ok ty -> Error (FunAppTyErr ty)
        | Error e -> Error e
      )
    | Let { name; ty = expected_ty; value; body; _ } -> (
        match typecheck env value with
        | Ok actual_ty when actual_ty = expected_ty ->
            typecheck (Env.add name expected_ty env) body
        | Ok actual_ty -> Error (LetTyErr (expected_ty, actual_ty))
        | Error e -> Error e
      )
    | Bop (op, e1, e2) -> (
        let (expected_ty1, expected_ty2, result_ty) = match op with
          | Add | Sub | Mul | Div | Mod -> (IntTy, IntTy, IntTy)
          | Lt | Lte | Gt | Gte | Eq | Neq -> (IntTy, IntTy, BoolTy)
          | And | Or -> (BoolTy, BoolTy, BoolTy)
        in
        match typecheck env e1 with
        | Error e -> Error e
        | Ok ty1 when ty1 <> expected_ty1 -> Error (OpTyErrL (op, expected_ty1, ty1))
        | Ok _ -> (
            match typecheck env e2 with
            | Error e -> Error e
            | Ok ty2 when ty2 <> expected_ty2 -> Error (OpTyErrR (op, expected_ty2, ty2))
            | Ok _ -> Ok result_ty
          )
      )
    | Assert e -> (
        match typecheck env e with
        | Ok BoolTy -> Ok UnitTy
        | Ok ty -> Error (AssertTyErr ty)
        | Error e -> Error e
      )
  in
  typecheck Env.empty e

let eval (expr : expr) : value =
  let rec go env = function
    | Unit -> VUnit
    | True -> VBool true
    | False -> VBool false
    | Num n -> VNum n
    | Var x -> Env.find x env
    | Let { is_rec; name; ty = _; value; body } -> (
        let extended_env =
          if is_rec then
            match value with
            | Fun (arg, _, body) ->
                let closure = VClos { name = Some name; arg; body; env } in
                Env.add name closure env
            | _ ->
                let gensym_arg = gensym () in
                let wrapped_body = Fun (gensym_arg, UnitTy, value) in
                let closure = VClos { name = Some name; arg = gensym_arg; body = wrapped_body; env } in
                Env.add name closure env
          else
            let v = go env value in
            Env.add name v env
        in
        go extended_env body
      )
    | Fun (arg, _, body) -> VClos { name = None; arg; body; env }
    | App (e1, e2) -> (
        match go env e1 with
        | VClos { name = Some fname; arg; body; env = closure_env } ->
            let v2 = go env e2 in
            let extended_env =
              Env.add fname (VClos { name = Some fname; arg; body; env = closure_env })
                (Env.add arg v2 closure_env)
            in
            go extended_env body
        | VClos { name = None; arg; body; env = closure_env } ->
            let v2 = go env e2 in
            let extended_env = Env.add arg v2 closure_env in
            go extended_env body
        | _ -> assert false
      )
    | If (cond, then_, else_) -> (
        match go env cond with
        | VBool true -> go env then_
        | VBool false -> go env else_
        | _ -> assert false 
      )
    | Bop (op, e1, e2) -> (
        match op with
        | And -> (
            match go env e1 with
            | VBool false -> VBool false 
            | VBool true -> go env e2 
            | _ -> assert false 
          )
        | Or -> (
            match go env e1 with
            | VBool true -> VBool true 
            | VBool false -> go env e2 
            | _ -> assert false 
          )
        | _ -> (
            let v1 = go env e1 in
            let v2 = go env e2 in
            match (v1, v2, op) with
            | (VNum n1, VNum n2, Add) -> VNum (n1 + n2)
            | (VNum n1, VNum n2, Sub) -> VNum (n1 - n2)
            | (VNum n1, VNum n2, Mul) -> VNum (n1 * n2)
            | (VNum n1, VNum n2, Div) -> if n2 = 0 then raise DivByZero else VNum (n1 / n2)
            | (VNum n1, VNum n2, Mod) -> if n2 = 0 then raise DivByZero else VNum (n1 mod n2)
            | (VNum n1, VNum n2, Lt) -> VBool (n1 < n2)
            | (VNum n1, VNum n2, Lte) -> VBool (n1 <= n2)
            | (VNum n1, VNum n2, Gt) -> VBool (n1 > n2)
            | (VNum n1, VNum n2, Gte) -> VBool (n1 >= n2)
            | (VNum n1, VNum n2, Eq) -> VBool (n1 = n2)
            | (VNum n1, VNum n2, Neq) -> VBool (n1 <> n2)
            | _ -> assert false 
          )
      )
    | Assert e -> (
        match go env e with
        | VBool true -> VUnit
        | VBool false -> raise AssertFail
        | _ -> assert false 
      )
  in
  go Env.empty expr

let interp (str : string) : (value, error) result =
  match parse str with
  | Some prog -> (
      let expr = desugar prog in
      match type_of expr with
      | Ok _ -> Ok (eval expr)
      | Error err -> Error err
    )
  | None -> Error ParseErr
