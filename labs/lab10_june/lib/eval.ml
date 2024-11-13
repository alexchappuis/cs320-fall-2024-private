
open Syntax
include Parser

let rec subst x e1 e2 =
  match e2 with 
  | Int _ -> e2
  | Bool _ -> e2
  | Var y -> if x = y then e1 else e2
  | UnaryOp (op, e21, e22) -> UnaryOP (op, subst x e1 e2')
  | BinaryOp (op, e2') -> 
    BinaryOP (op, subst x e1 e21, subst x e1 e22)
  | Fun (y, body) ->
    if x = y then e2 else Fun(y subst x e1 body)
  | App (e21, e22) -> App (subst x e1 e21, subst x e1 e22)
  | Let (y, e21, e22) -> 
    if x = y then e2 
    else Let ( y, subst x e1 e21, subst x e1 e22)
  | Ifte (e21, e22, e23) ->
    Ifte (subst x e1 e21, subst x e
  | _ -> _

(* Think of the return `v` of `eval m` as `m ⇓ v` *)
let rec eval e =
  match e with
  | Int i -> Int i
  | Bool b -> Bool b
  | UnaryOp (op, e') ->(
    let v = eval e' in 
    match op, v with
    | Neg, Int i -> Int ( -i )
    | Not, Bool b -> Bool (not b)
    | _ -> failwith "type error in unary op" )
  | failwith "type"
  | BinaryOp (op, e1, e2) -> (
      let v1 = eval e1 in
      let v2 = eval e2 in
      match op, v1, v2 with
      | Add, Int i1, Int i2 -> Int (i1 + i2) 
      | Sub, Int i1, Int i2 -> Int (i1 - i2) 
      | Mul, Int i1, Int i2 -> Int (i1 * i2) 
      | Div, Int i1, Int i2 -> Int (i1 / i2) 
      | Lt, Int i1, Int i2 -> Bool (i1 < i2) 
      | Gt, Int i1, Int i2 -> Bool (i1 > i2) 
      | Lte, Int i1, Int i2 -> Bool (i1 <= i2) 
      | Gte, Int i1, Int i2 -> Bool (i1 >= i2) 
      | Eq, Int i1, Int i2 -> Bool (i1 = i2)
      | Neq, Int i1, Int i2 -> Bool (i1 <> i2)  
      | And, Int i1, Int i2 -> Bool (i1 && i2)  
      | Or, Int i1, Int i2 -> Bool (i1 || i2)  
      | _ -> failwith "type error in binary op"
  )
  | Var x -> failwith "undeclared variable"
  | Fun (x, body) -> Fun (x, body))
  | App (e1, e2) ->
    (let v1 = eval e1 in 
    let v2 = eval e2 in
    match v1 with
    | Fun (x, body) ->
    | eval (subst x v2 body)
    | _ -> failwith "type error in operation"

  | _ -> failwith "unimplemented"


let interp fname =
  let c = open_in fname in
  let m = Parser.parse c in
  let v = eval m in
  v
