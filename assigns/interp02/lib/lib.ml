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

type ctx = (string * ty) list

(* let rec pp_ctx fmt (ctx : ctx) =
  Format.fprintf fmt "[";
  List.iter (fun (x, v) -> Format.fprintf fmt "(%s, %a); " x pp_ty v) ctx;
  Format.fprintf fmt "]" *)

let ty_equal t1 t2 =
  if t1 = t2 then ()
  else failwith "type error"

let rec infer (ctx : ctx) (e : expr) : ty =
  match e with
  | Unit -> UnitTy
  | True | False -> BoolTy
  | Num _ -> IntTy
  | Var x -> (
      try List.assoc x ctx with
      | Not_found -> failwith ("undefined variable " ^ x)
    )
  | If (cond, then_, else_) ->
      let t_cond = infer ctx cond in
      ty_equal t_cond BoolTy;
      let t_then = infer ctx then_ in
      let t_else = infer ctx else_ in
      ty_equal t_then t_else;
      t_then
  | Fun (arg, arg_ty, body) ->
      let t_body = infer ((arg, arg_ty) :: ctx) body in
      FunTy (arg_ty, t_body)
  | App (e1, e2) -> (
      let t_fn = infer ctx e1 in
      match t_fn with
      | FunTy (arg_ty, ret_ty) ->
          let t_arg = infer ctx e2 in
          ty_equal arg_ty t_arg;
          ret_ty
      | _ -> failwith "type error in function application"
    )
  | Let { is_rec; name; ty = expected_ty; value; body } ->
      let t_value = infer ctx value in
      ty_equal expected_ty t_value;
      let extended_ctx = (name, expected_ty) :: ctx in
      infer extended_ctx body
  | Bop (op, e1, e2) -> (
      let (expected_ty1, expected_ty2, result_ty) = match op with
        | Add | Sub | Mul | Div | Mod -> (IntTy, IntTy, IntTy)
        | Lt | Lte | Gt | Gte | Eq | Neq -> (IntTy, IntTy, BoolTy)
        | And | Or -> (BoolTy, BoolTy, BoolTy)
      in
      let t1 = infer ctx e1 in
      ty_equal t1 expected_ty1;
      let t2 = infer ctx e2 in
      ty_equal t2 expected_ty2;
      result_ty
    )
  | Assert e ->
      let t = infer ctx e in
      ty_equal t BoolTy;
      UnitTy

let type_of (e : expr) : (ty, error) result =
  try Ok (infer [] e) with
  | Failure msg -> Error (ParseErr) 
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
