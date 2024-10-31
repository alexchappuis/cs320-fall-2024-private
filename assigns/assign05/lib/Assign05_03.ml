type ident = string

type ty = 
  | Unit
  | Arr of ty * ty

type expr = 
  | Var of ident
  | Fun of ident * ty * expr
  | App of expr * expr

type ctxt = (ident * ty) list

let rec lookup x gamma =
  match gamma with
  | [] -> None
  | (y, t) :: rest -> if x = y then Some t else lookup x rest

let rec type_of gamma e = 
  match e with
  | Var x -> lookup x gamma
  | Fun (x, t1, e1) -> (
      match type_of ((x, t1) :: gamma) e1 with
      | Some t2 -> Some (Arr (t1, t2)) (* function type: t1 -> t2 *)
      | None -> None
    )
  | App (e1, e2) -> (
      match type_of gamma e1, type_of gamma e2 with
      | Some (Arr (t_arg, t_res)), Some t2 when t_arg = t2 -> Some t_res
      | _ -> None
    )
