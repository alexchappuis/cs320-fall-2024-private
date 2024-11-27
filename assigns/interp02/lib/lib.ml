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


let rec type_of (ctxt : (string * ty) list) (e : expr) : (ty, error) result =
  let rec go = function
    | Unit -> Ok UnitTy
    | True | False -> Ok BoolTy
    | Num _ -> Ok IntTy
    | Var x -> (
        match List.assoc_opt x ctxt with
        | Some ty -> Ok ty
        | None -> Error (UnknownVar x)
      )
    | If (e1, e2, e3) -> (
        match (go e1, go e2, go e3) with
        | Ok BoolTy, Ok t2, Ok t3 when t2 = t3 -> Ok t2
        | Ok BoolTy, Ok t2, Ok t3 -> Error (IfTyErr (t2, t3))
        | Ok ty, _, _ -> Error (IfCondTyErr ty)
        | Error err, _, _ -> Error err
        | _, Error err, _ -> Error err
        | _, _, Error err -> Error err
      )
    | Fun (x, ty, body) -> (
        match type_of ((x, ty) :: ctxt) body with
        | Ok ty_out -> Ok (FunTy (ty, ty_out))
        | Error err -> Error err
      )
    | App (e1, e2) -> (
        match (go e1, go e2) with
        | Ok (FunTy (ty_arg, ty_out)), Ok t2 when ty_arg = t2 -> Ok ty_out
        | Ok (FunTy (ty_arg, _)), Ok t2 -> Error (FunArgTyErr (ty_arg, t2))
        | Ok ty, _ -> Error (FunAppTyErr ty)
        | Error err, _ -> Error err
        | _, Error err -> Error err
      )
    | Let { is_rec; name; ty; value; body } -> (
        match type_of ctxt value with
        | Ok t1 when t1 = ty -> type_of ((name, ty) :: ctxt) body
        | Ok t1 -> Error (LetTyErr (ty, t1))
        | Error err -> Error err
      )
    | Bop (op, e1, e2) ->
        let op_expected_ty =
          match op with
          | Add | Sub | Mul | Div | Mod | Lt | Lte | Gt | Gte -> IntTy
          | And | Or -> BoolTy
          | Eq | Neq -> IntTy
        in
        (
          match (go e1, go e2) with
          | Ok ty1, Ok ty2 when ty1 = ty2 && ty1 = op_expected_ty -> Ok (if op = Eq || op = Neq then BoolTy else ty1)
          | Ok ty1, Ok ty2 ->
              if ty1 <> op_expected_ty then Error (OpTyErrL (op, op_expected_ty, ty1))
              else Error (OpTyErrR (op, op_expected_ty, ty2))
          | Error err, _ -> Error err
          | _, Error err -> Error err
        )
    | Assert e -> (
        match go e with
        | Ok BoolTy -> Ok UnitTy
        | Ok ty -> Error (AssertTyErr ty)
        | Error err -> Error err
      )
  in
  go e

let type_of (e : expr) : (ty, error) result = type_of [] e

let eval (expr : expr) : value option =
  let rec go env = function
    | Unit -> Some VUnit
    | True -> Some (VBool true)
    | False -> Some (VBool false)
    | Num n -> Some (VNum n)
    | Var x -> Env.find_opt x env
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
            match v with
            | Some v -> Env.add name v env
            | None -> env
        in
        go extended_env body
      )
    | Fun (arg, _, body) -> Some (VClos { name = None; arg; body; env })
    | App (e1, e2) -> (
        match go env e1 with
        | Some (VClos { name = Some fname; arg; body; env = closure_env }) -> (
            match go env e2 with
            | Some v2 ->
                let extended_env =
                  Env.add fname (VClos { name = Some fname; arg; body; env = closure_env })
                    (Env.add arg v2 closure_env)
                in
                go extended_env body
            | _ -> None
          )
        | Some (VClos { name = None; arg; body; env = closure_env }) -> (
            match go env e2 with
            | Some v2 ->
                let extended_env = Env.add arg v2 closure_env in
                go extended_env body
            | _ -> None
          )
        | _ -> None
      )
    | If (cond, then_, else_) -> (
        match go env cond with
        | Some (VBool true) -> go env then_
        | Some (VBool false) -> go env else_
        | _ -> None
      )
    | Bop (op, e1, e2) -> (
        match op with
        | And -> (
            match go env e1 with
            | Some (VBool false) -> Some (VBool false)
            | Some (VBool true) -> go env e2
            | _ -> None
          )
        | Or -> (
            match go env e1 with
            | Some (VBool true) -> Some (VBool true)
            | Some (VBool false) -> go env e2
            | _ -> None
          )
        | _ -> (
            let v1 = go env e1 in
            let v2 = go env e2 in
            match (v1, v2, op) with
            | (Some (VNum n1), Some (VNum n2), Add) -> Some (VNum (n1 + n2))
            | (Some (VNum n1), Some (VNum n2), Sub) -> Some (VNum (n1 - n2))
            | (Some (VNum n1), Some (VNum n2), Mul) -> Some (VNum (n1 * n2))
            | (Some (VNum n1), Some (VNum n2), Div) -> if n2 = 0 then None else Some (VNum (n1 / n2))
            | (Some (VNum n1), Some (VNum n2), Mod) -> if n2 = 0 then None else Some (VNum (n1 mod n2))
            | (Some (VNum n1), Some (VNum n2), Lt) -> Some (VBool (n1 < n2))
            | (Some (VNum n1), Some (VNum n2), Lte) -> Some (VBool (n1 <= n2))
            | (Some (VNum n1), Some (VNum n2), Gt) -> Some (VBool (n1 > n2))
            | (Some (VNum n1), Some (VNum n2), Gte) -> Some (VBool (n1 >= n2))
            | (Some (VNum n1), Some (VNum n2), Eq) -> Some (VBool (n1 = n2))
            | (Some (VNum n1), Some (VNum n2), Neq) -> Some (VBool (n1 <> n2))
            | _ -> None
          )
      )
    | Assert e -> (
        match go env e with
        | Some (VBool true) -> Some VUnit
        | Some (VBool false) -> None
        | _ -> None
      )
  in
  go Env.empty expr

let interp (str : string) : (value, error) result =
  match parse str with
  | Some prog -> (
      let expr = desugar prog in
      match type_of expr with
      | Ok _ -> (
          match eval expr with
          | Some v -> Ok v
          | None -> Error ParseErr
        )
      | Error err -> Error err
    )
  | None -> Error ParseErr
