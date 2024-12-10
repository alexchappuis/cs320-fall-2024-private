open Utils

exception AssertFail
exception DivByZero
exception RecWithoutArg
exception CompareFunVals

let parse = My_parser.parse

let rec ty_subst t x = function
  | TInt | TBool | TFloat | TUnit as base -> base
  | TVar y -> if y = x then t else TVar y
  | TList ty -> TList (ty_subst t x ty)
  | TOption ty -> TOption (ty_subst t x ty)
  | TPair (t1,t2) -> TPair (ty_subst t x t1, ty_subst t x t2)
  | TFun (t1,t2) -> TFun (ty_subst t x t1, ty_subst t x t2)

let ty_subst_c t x (t1, t2) = (ty_subst t x t1, ty_subst t x t2)
let ty_subst_cs t x = List.map (ty_subst_c t x)

let rec fvs = function
  | TInt | TBool | TFloat | TUnit -> VarSet.empty
  | TVar x -> VarSet.of_list [x]
  | TList ty | TOption ty -> fvs ty
  | TPair (t1,t2) | TFun (t1,t2) ->
    VarSet.union (fvs t1) (fvs t2)

let apply_subs subs ty =
  List.fold_left (fun acc (x, t) -> ty_subst t x acc) ty subs

let generalize ty =
  let vars = VarSet.to_list (fvs ty) in
  Forall (vars, ty)

let instantiate (Forall(vars, t)) =
  let subs = List.map (fun v -> (v, TVar (gensym ()))) vars in
  apply_subs subs t

let unify (tau:ty) (cs:constr list) : ty_scheme option =
  let rec unify_cs subs = function
    | [] -> Some subs
    | (u,v)::rest ->
      let u = apply_subs subs u in
      let v = apply_subs subs v in
      if u = v then unify_cs subs rest
      else match u,v with
        | TFun (a1,a2), TFun(b1,b2) ->
          unify_cs subs ((a1,b1)::(a2,b2)::rest)
        | TPair(a1,a2), TPair(b1,b2) ->
          unify_cs subs ((a1,b1)::(a2,b2)::rest)
        | TList a, TList b ->
          unify_cs subs ((a,b)::rest)
        | TOption a, TOption b ->
          unify_cs subs ((a,b)::rest)
        | TVar x, t | t, TVar x ->
          if VarSet.mem x (fvs t) then None
          else
            let new_subs = (x,t)::subs in
            let updated_rest = List.map (fun (r1,r2) ->
              (apply_subs [(x,t)] r1, apply_subs [(x,t)] r2)) rest
            in
            unify_cs new_subs updated_rest
        | _ -> None
  in
  match unify_cs [] ((TVar "$_out",tau)::cs) with
  | None -> None
  | Some subs ->
    let tau' = apply_subs subs (TVar "$_out") in
    Some (generalize tau')

let fresh_ty () = TVar (gensym ())

let rec typeof' (ctxt:stc_env) (e:expr) : (ty * constr list) =
  match e with
  | Unit -> (TUnit,[])
  | True | False -> (TBool,[])
  | Int _ -> (TInt,[])
  | Float _ -> (TFloat,[])
  | ENone ->
    let alpha = fresh_ty () in
    (TOption alpha, [])
  | ESome e ->
    let (t,c) = typeof' ctxt e in
    (TOption t,c)
  | Nil ->
    let alpha = fresh_ty () in
    (TList alpha, [])
  | Var x ->
    let scheme = Env.find x ctxt in
    (instantiate scheme, [])
  | Annot(e,ty) ->
    let (t',c) = typeof' ctxt e in
    (ty,(t',ty)::c)
  | Assert e ->
    begin match e with
    | False ->
      let alpha = fresh_ty () in
      (alpha,[])
    | _ ->
      let (t,c) = typeof' ctxt e in
      (TUnit,(t,TBool)::c)
    end
  | If(c,t,f) ->
    let (tc,cc) = typeof' ctxt c in
    let (tt,ct_) = typeof' ctxt t in
    let (tf,cf) = typeof' ctxt f in
    (tf,(tc,TBool)::(tt,tf)::cc@ct_@cf)
  | Bop(op,e1,e2) ->
    let (t1,c1) = typeof' ctxt e1 in
    let (t2,c2) = typeof' ctxt e2 in
    let result_type, extra =
      match op with
      | Add|Sub|Mul|Div|Mod -> (TInt,[(t1,TInt);(t2,TInt)])
      | AddF|SubF|MulF|DivF|PowF -> (TFloat,[(t1,TFloat);(t2,TFloat)])
      | Lt|Lte|Gt|Gte|Eq|Neq -> (TBool,[(t1,t2)])
      | And|Or -> (TBool,[(t1,TBool);(t2,TBool)])
      | Cons ->
        (TList t1,[(t2,TList t1)])
      | Concat ->
        let alpha = fresh_ty () in
        (TList alpha,[(t1,TList alpha);(t2,TList alpha)])
      | Comma ->
        (TPair(t1,t2),[])
    in
    (result_type, extra@c1@c2)
  | Fun(arg,ty_opt,body) ->
    let arg_ty = match ty_opt with Some ty -> ty | None -> fresh_ty () in
    let ctxt' = Env.add arg (Forall([],arg_ty)) ctxt in
    let (t_body,c_body) = typeof' ctxt' body in
    (TFun(arg_ty,t_body), c_body)
  | App(e1,e2) ->
    let (t1,c1) = typeof' ctxt e1 in
    let (t2,c2) = typeof' ctxt e2 in
    let alpha = fresh_ty () in
    (alpha,(t1,TFun(t2,alpha))::c1@c2)
  | Let {is_rec; name; value; body} ->
    if is_rec then
      let alpha = fresh_ty () in
      let ctxt' = Env.add name (Forall([],alpha)) ctxt in
      let (t_val,c_val) = typeof' ctxt' value in
      let (t_body,c_body) = typeof' (Env.add name (Forall([],alpha)) ctxt) body in
      (t_body,(alpha,t_val)::c_val@c_body)
    else
      let (t_val,c_val) = typeof' ctxt value in
      let ctxt' = Env.add name (Forall([],t_val)) ctxt in
      let (t_body,c_body) = typeof' ctxt' body in
      (t_body, c_val@c_body)
  | ListMatch {matched; hd_name; tl_name; cons_case; nil_case} ->
    let (t_m,c_m) = typeof' ctxt matched in
    let alpha = fresh_ty () in
    let ctxt_cons = Env.add hd_name (Forall([],alpha))
        (Env.add tl_name (Forall([],TList alpha)) ctxt)
    in
    let (t_cons,c_cons) = typeof' ctxt_cons cons_case in
    let (t_nil,c_nil) = typeof' ctxt nil_case in
    (t_nil,(t_m,TList alpha)::(t_cons,t_nil)::c_m@c_cons@c_nil)
  | OptMatch {matched; some_name; some_case; none_case} ->
    let (t_m,c_m) = typeof' ctxt matched in
    let alpha = fresh_ty () in
    let ctxt_some = Env.add some_name (Forall([],alpha)) ctxt in
    let (t_some,c_some) = typeof' ctxt_some some_case in
    let (t_none,c_none) = typeof' ctxt none_case in
    (t_none,(t_m,TOption alpha)::(t_some,t_none)::c_m@c_some@c_none)
  | PairMatch {matched; fst_name; snd_name; case} ->
    let (t_m,c_m) = typeof' ctxt matched in
    let alpha = fresh_ty () in
    let beta = fresh_ty () in
    let ctxt_pair = Env.add fst_name (Forall([],alpha))
        (Env.add snd_name (Forall([],beta)) ctxt)
    in
    let (t_case,c_case) = typeof' ctxt_pair case in
    (t_case,(t_m,TPair(alpha,beta))::c_m@c_case)

let type_of (ctxt:stc_env) (e:expr) : ty_scheme option =
  let (t,c) = typeof' ctxt e in
  unify t c

let type_check (p:prog) : ty_scheme option =
  let rec go ctxt = function
    | [] -> None
    | [top] ->
      let scheme_opt = type_of ctxt top.value in
      scheme_opt
    | top::rest ->
      match type_of ctxt top.value with
      | None -> None
      | Some s ->
        let Forall(_,ty) = s in
        let ctxt' = Env.add top.name s ctxt in
        go ctxt' rest
  in go Env.empty p

let rec cmp_val v1 v2 =
  match v1,v2 with
  | VInt i1,VInt i2 -> compare i1 i2
  | VFloat f1,VFloat f2 -> compare f1 f2
  | VBool b1,VBool b2 -> compare b1 b2
  | VUnit,VUnit -> 0
  | VNone,VNone -> 0
  | VSome x,VSome y -> cmp_val x y
  | VList l1,VList l2 ->
    let rec cmp_lists a b =
      match a,b with
      | [],[] -> 0
      | [],_ -> -1
      | _,[] -> 1
      | x::xs,y::ys ->
        let c = cmp_val x y in
        if c=0 then cmp_lists xs ys else c
    in cmp_lists l1 l2
  | VPair(a1,a2),VPair(b1,b2) ->
    let c = cmp_val a1 b1 in
    if c=0 then cmp_val a2 b2 else c
  | VClos _,_ | _,VClos _ -> raise CompareFunVals
  | _ -> failwith "Comparison of incompatible types"

let rec eval_expr (env:dyn_env) (e:expr) : value =
  let eval e = eval_expr env e in
  match e with
  | Unit -> VUnit
  | True -> VBool true
  | False -> VBool false
  | Int n -> VInt n
  | Float f -> VFloat f
  | Nil -> VList []
  | ENone -> VNone
  | ESome e -> VSome (eval e)
  | Var x -> Env.find x env
  | Assert e ->
    begin match e with
    | False -> raise AssertFail
    | _ ->
      let v = eval e in
      match v with
      | VBool true -> VUnit
      | VBool false -> raise AssertFail
      | _ -> failwith "assert: non-boolean"
    end
  | If(c,t,f) ->
    begin match eval c with
    | VBool true -> eval t
    | VBool false -> eval f
    | _ -> failwith "If condition not bool"
    end
  | Bop(op,e1,e2) ->
    let v1 = eval e1 in
    let v2 = eval e2 in
    let int_op f =
      match v1,v2 with
      | VInt i1, VInt i2 ->
        (try VInt (f i1 i2) with Division_by_zero -> raise DivByZero)
      | _ -> failwith "Type error"
    in
    let float_op f =
      match v1,v2 with
      | VFloat f1, VFloat f2 ->
        (try VFloat (f f1 f2) with Division_by_zero -> raise DivByZero)
      | _ -> failwith "Type error"
    in
    begin match op with
    | Add -> int_op (fun x y -> x+y)
    | Sub -> int_op (fun x y -> x-y)
    | Mul -> int_op (fun x y -> x*y)
    | Div -> int_op (fun x y -> if y=0 then raise DivByZero else x/y)
    | Mod -> int_op (fun x y -> if y=0 then raise DivByZero else x mod y)
    | AddF -> float_op ( +. )
    | SubF -> float_op ( -. )
    | MulF -> float_op ( *. )
    | DivF -> float_op (fun x y -> if y=0.0 then raise DivByZero else x /. y)
    | PowF -> float_op ( ** )
    | Lt -> VBool (cmp_val v1 v2 < 0)
    | Lte -> VBool (cmp_val v1 v2 <= 0)
    | Gt -> VBool (cmp_val v1 v2 > 0)
    | Gte -> VBool (cmp_val v1 v2 >= 0)
    | Eq -> VBool (cmp_val v1 v2 = 0)
    | Neq -> VBool (cmp_val v1 v2 <> 0)
    | And ->
      (match v1,v2 with VBool b1,VBool b2 -> VBool (b1 && b2) | _ -> failwith "Type error")
    | Or ->
      (match v1,v2 with VBool b1,VBool b2 -> VBool (b1 || b2) | _ -> failwith "Type error")
    | Cons ->
      (match v2 with
       | VList l -> VList (v1::l)
       | _ -> failwith "Type error")
    | Concat ->
      (match v1,v2 with
       | VList l1, VList l2 -> VList (l1 @ l2)
       | _ -> failwith "Type error")
    | Comma ->
      VPair(v1,v2)
    end
  | Fun(arg,_,body) ->
    VClos {name=None;arg;body;env}
  | App(e1,e2) ->
    let v1 = eval e1 in
    let v2 = eval e2 in
    begin match v1 with
    | VClos {name;arg;body;env=env'} ->
      let env'' =
        match name with
        | None -> Env.add arg v2 env'
        | Some f -> Env.add f v1 (Env.add arg v2 env')
      in
      eval_expr env'' body
    | _ -> failwith "Application to non-function"
    end
  | Let {is_rec=false; name; value; body} ->
    let v_val = eval value in
    let env' = Env.add name v_val env in
    eval_expr env' body
  | Let {is_rec=true; name; value; body} ->
    begin match value with
    | Fun(arg,_,fbody) ->
      let rec_clos = VClos {name=Some name; arg; body=fbody; env} in
      let env' = Env.add name rec_clos env in
      eval_expr env' body
    | _ ->
      raise RecWithoutArg
    end
  | ListMatch {matched; hd_name; tl_name; cons_case; nil_case} ->
    let vm = eval matched in
    begin match vm with
    | VList(h::t) ->
      let env' = Env.add hd_name h (Env.add tl_name (VList t) env) in
      eval_expr env' cons_case
    | VList [] ->
      eval_expr env nil_case
    | _ -> failwith "list match on non-list"
    end
  | OptMatch {matched; some_name; some_case; none_case} ->
    let vm = eval matched in
    begin match vm with
    | VSome x ->
      let env' = Env.add some_name x env in
      eval_expr env' some_case
    | VNone ->
      eval_expr env none_case
    | _ -> failwith "option match on non-option"
    end
  | PairMatch {matched; fst_name; snd_name; case} ->
    let vm = eval matched in
    begin match vm with
    | VPair(vf,vs) ->
      let env' = Env.add fst_name vf (Env.add snd_name vs env) in
      eval_expr env' case
    | _ -> failwith "pair match on non-pair"
    end

let eval (p:prog) : value =
  let rec go env = function
    | [] -> VUnit
    | [top] ->
      let v = eval_expr env top.value in
      v
    | top::rest ->
      let v = eval_expr env top.value in
      let env' = Env.add top.name v env in
      go env' rest
  in go Env.empty p

let interp (s:string) : (value * ty_scheme, error) result =
  match parse s with
  | None -> Error ParseError
  | Some prog ->
    begin match type_check prog with
    | None -> Error TypeError
    | Some scheme ->
      let v = eval prog in
      Ok (v, scheme)
    end
