let sign n =
  if n > 0 then 1
  else if n < 0 then -1
  else 0

let rec drop n lst =
  match lst with
  | [] -> []
  | _ :: tl ->
      if n <= 0 then lst  
      else drop (n - 1) tl  

let is_valid_list lst =
  let rec check_sublists lst prev_sign =
    match lst with
    | [] -> true  
    | 0 :: tl -> check_sublists tl prev_sign 
    | hd :: tl when sign hd = 0 -> check_sublists tl prev_sign 
    | hd :: tl ->
        let current_sign = sign hd in
        if prev_sign = 0 || prev_sign <> current_sign then
          let rec get_samesign_sublist lst curr_sign =
            match lst with
            | hd :: tl when sign hd = curr_sign -> hd :: get_samesign_sublist tl curr_sign
            | _ -> []
          in
          let sublist = get_samesign_sublist (hd :: tl) current_sign in
          let remaining = drop (List.length sublist) (hd :: tl) in
          check_sublists remaining current_sign
        else
          false  
  in
  check_sublists lst 0  

let split_by_zero lst =
  let rec aux current acc = function
    | [] ->
        if current = [] then List.rev acc 
        else List.rev (List.rev current :: acc)
    | 0 :: tl ->
        if current = [] then aux [] acc tl 
        else aux [] (List.rev current :: acc) tl
    | hd :: tl -> aux (hd :: current) acc tl  
  in
  aux [] [] lst 

let group l =
  if is_valid_list l then
    Some (split_by_zero l) 
  else
    None 