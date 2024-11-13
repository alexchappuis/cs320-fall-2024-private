type expr =
  | Var of string
  | Fun of string * expr
  | App of expr * expr
  | Let of string * expr * expr

type prog = (string * expr) list

type value =
  | VFun of string * expr


(* evlautaion funciton is just implementing the rules of the semantic derevation*)

let rec eval e =
  match e with
  | Var x -> None
  | Fun(x, body) -> Some(VFun (x, e))
  | App(e1, e2) -> (
      match eval e1 with 
      | Some (VFun(x,e)) -> (
        match eval e2 with
        | some v2 -> evanl (subst v2 x e)
        | _ -> None
      )
  )

  let rec subst v x e = 
    match e with
    | var y -> if x = y then v else Var x
    | App(e1, e2) -> 
      App(subst v x1 e1, subst v x e2)
      fun z -> [v/x]([z/y]e)

let rec var_replace y x e = 
  match e with  
  | Var z -> 
    if z = x then Var y else Var z
    | App(e1,e2) ->
      App(var replace y x e1, var replace y x e2)
    | Fun (z,e) ->
        Fun
