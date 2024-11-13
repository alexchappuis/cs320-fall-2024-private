let rec add_to_int_list n xs =
  match xs with 
  | [] -> []
  | x :: xs -> x + n :: add_to_int_list n xs



  let rec mult_int_list n xs =
    match xs with
    | [] -> []
    | x :: xs -> x * n :: mult_int_list n xs

    let rec map f xs = 
      match xs with
      | [] -> []
      | x :: xs -> f x :: map f xs


    let add_to_int_list n xs = 
      map( fun x -> x + n) xs

    
    let sum xs =
      let rec go acc xs = 
        match xs with
        | [] -> acc
        | x :: xs -> go (acc + x) xs
      in 
        go 0 xs
        
let join_strings seperator xs =
  let rec go acc xs =
    match xs with
    | [] -> acc
    | x :: xs -> go (acc ^ x ^ seperator) xs
  in 
    go "" xs


    let _ print_endline ( join_strings ())


let rec fold f acc xs =
  match xs with
  | [] -> acc
  | x :: xs -> fold f (f acc xs) xs

  let join_strings seperator xs =
    fold ( fun x -> x ^ seperator) "" xs