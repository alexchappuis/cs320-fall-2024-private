open Utils
let parse (tokens: tok list) : expr option =
  let rec process tokens stack =
    match tokens, stack with
    | [], [final_expr] -> Some final_expr 
    | [], _ -> None 
    | TNum n :: rest, _ -> 
        process rest (Num n :: stack) 
    | TAdd :: rest, rhs :: lhs :: stack' ->
        process rest (Add (lhs, rhs) :: stack') 
    | TLt :: rest, rhs :: lhs :: stack' ->
        process rest (Lt (lhs, rhs) :: stack') 
    | TIte :: rest, else_expr :: then_expr :: cond :: stack' ->
        process rest (Ite (cond, then_expr, else_expr) :: stack') 
    | _ -> None  
  in
  process tokens []  