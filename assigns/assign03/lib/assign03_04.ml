let determine_sign x =
  if x > 0 then 1
  else if x < 0 then -1
  else 0

let rec remove n list =
  match list with
  | [] -> []
  | _ :: tail ->
      if n <= 0 then list
      else remove (n - 1) tail

let is_list_valid list =
  let rec verify_sublists list last_sign =
    match list with
    | [] -> true
    | 0 :: tail -> 
        if last_sign = 0 then false (* 2 0s in a row are invalid *)
        else verify_sublists tail 0
    | head :: tail when determine_sign head = 0 -> verify_sublists tail last_sign
    | head :: tail ->
        let current_sign = determine_sign head in
        if last_sign = 0 || last_sign <> current_sign then
          verify_sublists (remove (List.length (gather_same_sign_sublist (head :: tail) current_sign)) (head :: tail)) current_sign
        else
          false
  and gather_same_sign_sublist sublist current_sign =
    match sublist with
    | head_sub :: tail_sub when determine_sign head_sub = current_sign -> head_sub :: gather_same_sign_sublist tail_sub current_sign
    | _ -> []
  in
  verify_sublists list 0

let partition_into_groups list =
  let rec helper_fun current acc = function
    | [] -> 
        if current = [] then List.rev acc
        else List.rev (List.rev current :: acc)
    | 0 :: tail ->
        if current = [] then helper_fun [] acc tail
        else helper_fun [] (List.rev current :: acc) tail
    | head :: tail -> helper_fun (head :: current) acc tail
  in
  helper_fun [] [] list

let group list =
  if is_list_valid list then
    Some (partition_into_groups list)
  else
    None