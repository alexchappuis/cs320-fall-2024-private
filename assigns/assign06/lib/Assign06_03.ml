open Utils

type ty = 
  | TInt
  | TBool

  let rec type_of (expr: expr) : ty option =
    match expr with
    | Num _ -> Some TInt  
    | Add (lhs, rhs) -> 
        (match type_of lhs, type_of rhs with
         | Some TInt, Some TInt -> Some TInt  
         | _ -> None) 
    | Lt (lhs, rhs) -> 
        (match type_of lhs, type_of rhs with
         | Some TInt, Some TInt -> Some TBool  
         | _ -> None)  
    | Ite (condition, expr1, expr2) -> 
        (match type_of condition, type_of expr1, type_of expr2 with
         | Some TBool, Some t1, Some t2 when t1 = t2 -> Some t1 
         | _ -> None)  (* If types don't match, type inference fails *)
