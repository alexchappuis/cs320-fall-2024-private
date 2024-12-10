open Utils
include My_parser


let rec ty_subst t x ty =
  match ty with
  | TUnit | TInt | TFloat | TBool -> ty
  | TVar y -> if y = x then t else ty
  | TList ty' -> TList (ty_subst t x ty')
  | TOption ty' -> TOption (ty_subst t x ty')
  | TPair (t1, t2) -> TPair (ty_subst t x t1, ty_subst t x t2)
  | TFun (t1, t2) -> TFun (ty_subst t x t1, ty_subst t x t2)


let apply_subs subs ty =
  List.fold_left (fun acc (x, t) -> ty_subst t x acc) ty subs

let rec fvs ty =
  match ty with
  | TUnit | TInt | TFloat | TBool -> VarSet.empty
  | TVar x -> Env.add x () Env.empty
  | TList t | TOption t -> fvs t
  | TPair (t1,t2) | TFun (t1,t2) ->
    VarSet.union (fvs t1) (fvs t2)

let generalize ty =
  let vars = VarSet.to_list (fvs ty) in
  Forall (vars, ty)

let instantiate (Forall(vars, t)) =
  let subs = List.map (fun v -> (v, TVar (gensym ()))) vars in
  apply_subs subs t

let unify (tau : ty) (cs : constr list) : ty_scheme option =
  let rec solve subs = function
    | [] ->
      let tau' = apply_subs subs tau in
      Some (generalize tau')
    | (u,v)::rest ->
      let u' = apply_subs subs u in
      let v' = apply_subs subs v in
      if u' = v' then
        solve subs rest
      else
        match (u', v') with
        | TFun (a1,a2), TFun (b1,b2) ->
          solve subs ((a1,b1)::(a2,b2)::rest)
        | TPair (a1,a2), TPair (b1,b2) ->
          solve subs ((a1,b1)::(a2,b2)::rest)
        | TList a, TList b ->
          solve subs ((a,b)::rest)
        | TOption a, TOption b ->
          solve subs ((a,b)::rest)
        | TVar x, t | t, TVar x ->
          if Env.mem x (fvs t) then None
          else
            solve ((x,t)::subs) rest
        | _ -> None
  in
  solve [] cs

let rec typeof' (ctxt : stc_env) (e : expr) : ty * constr list =
  let fresh () = TVar (gensym ()) in
  match e with
  | Unit -> (TUnit, [])
  | True | False -> (TBool, [])
  | Int _ -> (TInt, [])
  | Float _ -> (TFloat, [])
  | Var x ->
    let sch = Env.find x ctxt in
    (instantiate sch, [])
  | ENone ->
    let alpha = fresh () in
    (TOption alpha, [])
  | ESome e1 ->
    let (t,c) = typeof' ctxt e1 in
    (TOption t, c)
  | Nil ->
    let alpha = fresh () in
    (TList alpha, [])
  | OptMatch {matched; some_name; some_case; none_case} ->
    let (t_m,c_m) = typeof' ctxt matched in
    let alpha = fresh () in
    let ctxt_some = Env.add some_name (Forall([],alpha)) ctxt in
    let (t_some,c_some) = typeof' ctxt_some some_case in
    let (t_none,c_none) = typeof' ctxt none_case in
    (t_none, (t_m, TOption alpha)::(t_some,t_none)::c_m@c_some@c_none)
  | ListMatch {matched; hd_name; tl_name; cons_case; nil_case} ->
    let (t_m,c_m) = typeof' ctxt matched in
    let alpha = fresh () in
    let ctxt_h = Env.add hd_name (Forall([],alpha)) ctxt in
    let ctxt_ht = Env.add tl_name (Forall([], TList alpha)) ctxt_h in
    let (t_cons,c_cons) = typeof' ctxt_ht cons_case in
    let (t_nil,c_nil) = typeof' ctxt nil_case in
    (t_nil, (t_m,TList alpha)::(t_cons,t_nil)::c_m@c_cons@c_nil)
  | PairMatch {matched; fst_name; snd_name; case} ->
    let (t_m,c_m) = typeof' ctxt matched in
    let alpha = fresh () in
    let beta = fresh () in
    let ctxt_f = Env.add fst_name (Forall([],alpha)) ctxt in
    let ctxt_fs = Env.add snd_name (Forall([],beta)) ctxt_f in
    let (t_case,c_case) = typeof' ctxt_fs case in
    (t_case,(t_m,TPair(alpha,beta))::c_m@c_case)
  | Fun (arg, ty_opt, body) ->
    let arg_ty = match ty_opt with None -> fresh () | Some t -> t in
    let ctxt' = Env.add arg (Forall([],arg_ty)) ctxt in
    let (t_body,c_body) = typeof' ctxt' body in
    (TFun(arg_ty,t_body), c_body)
  | App (e1,e2) ->
    let (t1,c1) = typeof' ctxt e1 in
    let (t2,c2) = typeof' ctxt e2 in
    let alpha = fresh () in
    (alpha,(t1,TFun(t2,alpha))::c1@c2)
  | Let {is_rec=false;name;value;body} ->
    let (t_val,c_val) = typeof' ctxt value in
    let ctxt' = Env.add name (Forall([],t_val)) ctxt in
    let (t_body,c_body) = typeof' ctxt' body in
    (t_body,c_val@c_body)
  | Let {is_rec=true; name; value; body} ->
    let alpha = fresh () in
    let beta = fresh () in
    let ctxt_f = Env.add name (Forall([], TFun(alpha,beta))) ctxt in
    let (t_val,c_val) = typeof' ctxt_f value in
    let ctxt_f2 = Env.add name (Forall([],TFun(alpha,beta))) ctxt in
    let (t_body,c_body) = typeof' ctxt_f2 body in
    (t_body,(t_val, TFun(alpha,beta))::c_val@c_body)
  | Assert False ->
    let alpha = fresh () in
    (alpha,[])
  | Assert e ->
    let (t,c) = typeof' ctxt e in
    (TUnit,(t,TBool)::c)
  | If (e1,e2,e3) ->
    let (t1,c1) = typeof' ctxt e1 in
    let (t2,c2) = typeof' ctxt e2 in
    let (t3,c3) = typeof' ctxt e3 in
    (t3,(t1,TBool)::(t2,t3)::c1@c2@c3)
  | Annot (e,ty) ->
    let (t',c) = typeof' ctxt e in
    (ty,(t',ty)::c)
  | Bop (op,e1,e2) ->
    let (t1,c1) = typeof' ctxt e1 in
    let (t2,c2) = typeof' ctxt e2 in
    let result, extra =
      match op with
      | Add|Sub|Mul|Div|Mod -> (TInt, [(t1,TInt);(t2,TInt)])
      | AddF|SubF|MulF|DivF|PowF -> (TFloat,[(t1,TFloat);(t2,TFloat)])
      | And|Or -> (TBool,[(t1,TBool);(t2,TBool)])
      | Eq|Neq|Lt|Lte|Gt|Gte ->
        (TBool,[(t1,t2)])
      | Cons ->
        (TList t1,[(t2,TList t1)])
      | Concat ->
        let alpha = fresh () in
        (TList alpha, [(t1,TList alpha);(t2,TList alpha)])
      | Comma -> (TPair(t1,t2),[])
    in
    (result,extra@c1@c2)

let type_of (ctxt: stc_env) (e: expr) : ty_scheme option =
  let (t,c) = typeof' ctxt e in
  unify t c


exception AssertFail
exception DivByZero
exception RecWithoutArg
exception CompareFunVals

let rec eval_expr (env:dyn_env) (e:expr) : value =
  let eval e = eval_expr env e in
  match e with
  | Unit -> VUnit
  | True -> VBool true
  | False -> VBool false
  | Int n -> VInt n
  | Float f -> VFloat f
  | ENone -> VNone
  | ESome e' -> VSome (eval e')
  | Nil -> VList []
  | Var x -> Env.find x env
  | Assert e' ->
    (match eval e' with
     | VBool true -> VUnit
     | VBool false -> raise AssertFail
     | _ -> raise AssertFail)
  | If (c,t,f) ->
    (match eval c with
     | VBool true -> eval t
     | VBool false -> eval f
     | _ -> failwith "impossible")
  | Bop (op,e1,e2) ->
    let v1 = eval e1 in
    let v2 = eval e2 in
    let int_op f = match v1,v2 with
      | VInt i1, VInt i2 -> VInt (f i1 i2)
      | _ -> failwith " impossible"
    in
    let float_op f = match v1,v2 with
      | VFloat f1, VFloat f2 -> VFloat (f f1 f2)
      | _ -> failwith "impossible"
    in
    begin match op with
    | Add -> int_op (fun x y->x+y)
    | Sub -> int_op (fun x y->x-y)
    | Mul -> int_op (fun x y->x*y)
    | Div -> (match v1,v2 with
        | VInt _, VInt 0 -> raise DivByZero
        | VInt x,VInt y -> VInt (x/y)
        | _ -> failwith "impossible")
    | Mod -> (match v1,v2 with
        | VInt _,VInt 0 -> raise DivByZero
        | VInt x,VInt y -> VInt(x mod y)
        | _ -> failwith "impossible")
    | AddF -> float_op ( +. )
    | SubF -> float_op ( -. )
    | MulF -> float_op ( *. )
    | DivF -> (match v1,v2 with
        | VFloat _,VFloat 0.0 -> raise DivByZero
        | VFloat x,VFloat y -> VFloat(x /. y)
        | _ -> failwith "impossible")
    | PowF -> float_op ( ** )
    | Lt | Lte | Gt | Gte | Eq | Neq ->
      let cmp_val v1 v2 =
        let fail_funvals () = raise CompareFunVals in
        let rec eqv a b =
          match a,b with
          | VInt i1,VInt i2 -> compare i1 i2
          | VFloat f1,VFloat f2 -> compare f1 f2
          | VBool b1,VBool b2 -> compare b1 b2
          | VUnit,VUnit -> 0
          | VNone,VNone -> 0
          | VSome x,VSome y -> eqv x y
          | VPair(x1,x2),VPair(y1,y2) ->
            let c = eqv x1 y1 in if c=0 then eqv x2 y2 else c
          | VList l1,VList l2 ->
            let rec cmp_lists l1 l2 =
              match l1,l2 with
              | [],[] -> 0
              | [],_ -> -1
              | _,[] -> 1
              | x::xs,y::ys ->
                let c = eqv x y in
                if c=0 then cmp_lists xs ys else c
            in cmp_lists l1 l2
          | VClos _,_ | _,VClos _ -> fail_funvals ()
          | _ -> failwith " types arent compatiable for comparison"
        in eqv v1 v2
      in
      let c = cmp_val v1 v2 in
      (match op with
       | Eq -> VBool (c=0)
       | Neq -> VBool (c<>0)
       | Lt -> VBool (c<0)
       | Lte -> VBool (c<=0)
       | Gt -> VBool (c>0)
       | Gte -> VBool (c>=0)
       | _ -> failwith "impossible")
    | And ->
      (match v1 with
       | VBool false -> VBool false
       | VBool true -> (match v2 with VBool b -> VBool b | _-> failwith "type error")
       | _ -> failwith "error")
    | Or ->
      (match v1 with
       | VBool true -> VBool true
       | VBool false -> (match v2 with VBool b->VBool b |_ ->failwith "type error")
       | _-> failwith "error")
    | Cons ->
      (match v2 with
       | VList l -> VList (v1::l)
       | _ -> failwith "error")
    | Concat ->
      (match v1,v2 with
       | VList l1, VList l2 -> VList (l1 @ l2)
       | _ -> failwith "error")
    | Comma ->
      VPair(v1,v2)
    end
  | Fun(arg,_,body) ->
    VClos {name=None; arg; body; env}
  | App(e1,e2) ->
    let v1 = eval e1 in
    let v2 = eval e2 in
    (match v1 with
     | VClos {name;arg;body;env=env'} ->
       let env'' = match name with
         | None -> Env.add arg v2 env'
         | Some f -> Env.add f v1 (Env.add arg v2 env')
       in
       eval_expr env'' body
     | _ -> failwith "error")
  | Let {is_rec=false; name; value; body} ->
    let v_val = eval value in
    eval_expr (Env.add name v_val env) body
  | Let {is_rec=true; name; value; body} ->
    (match value with
     | Fun(arg,_,bdy) ->
       let clos = VClos {name=Some name; arg; body=bdy; env} in
       eval_expr (Env.add name clos env) body
     | _ -> raise RecWithoutArg)
  | ListMatch {matched; hd_name; tl_name; cons_case; nil_case} ->
    (match eval matched with
     | VList(h::t) ->
       eval_expr (Env.add tl_name (VList t) (Env.add hd_name h env)) cons_case
     | VList [] ->
       eval_expr env nil_case
     | _ -> failwith "Expected list in match")
  | OptMatch {matched; some_name; some_case; none_case} ->
    (match eval matched with
     | VSome v ->
       eval_expr (Env.add some_name v env) some_case
     | VNone ->
       eval_expr env none_case
     | _-> failwith "Expected option in match")
  | PairMatch {matched; fst_name; snd_name; case} ->
    (match eval matched with
     | VPair(vf,vs) ->
       eval_expr (Env.add snd_name vs (Env.add fst_name vf env)) case
     | _ -> failwith "Expected pair in match")
  | Annot (e,_) -> eval e


let type_check =
  let rec go ctxt = function
  | [] -> Some (Forall ([], TUnit))
  | {is_rec;name;value} :: ls ->
    match type_of ctxt (Let {is_rec;name;value; body = Var name}) with
    | Some ty -> (
      match ls with
      | [] -> Some ty
      | _ ->
        let ctxt = Env.add name ty ctxt in
        go ctxt ls
    )
    | None -> None
  in go Env.empty

let eval p =
  let rec nest = function
    | [] -> Unit
    | [{is_rec;name;value}] -> Let {is_rec;name;value;body = Var name}
    | {is_rec;name;value} :: ls -> Let {is_rec;name;value;body = nest ls}
  in eval_expr Env.empty (nest p)

let interp input =
  match parse input with
  | Some prog -> (
    match type_check prog with
    | Some ty -> Ok (eval prog, ty)
    | None -> Error TypeError
  )
  | None -> Error ParseError
