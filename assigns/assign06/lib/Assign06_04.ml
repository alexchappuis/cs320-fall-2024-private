open Utils

type value = 
  | VNum of int
  | VBool of bool

  let rec eval (expr: expr) : value =  
    match expr with
    | Num n -> VNum n  
    | Add (lhs, rhs) -> 
        (match eval lhs, eval rhs with
         | VNum n1, VNum n2 -> VNum (n1 + n2)
         | _ -> failwith "needs 2 numbers")

    | Lt (lhs, rhs) ->  
        (match eval lhs, eval rhs with
         | VNum n1, VNum n2 -> VBool (n1 < n2)
         | _ -> failwith "needs to be 2 numbers")

    | Ite (cond, then_expr, else_expr) -> 
        (match eval cond with
         | VBool true -> eval then_expr  
         | VBool false -> eval else_expr
         |  _ -> failwith "condition should be boolean" )
