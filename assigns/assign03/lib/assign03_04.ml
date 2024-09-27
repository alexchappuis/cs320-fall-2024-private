let group lst =
  let rec aux current_group acc sign = function
    | [] -> 
        if current_group = [] then Some (List.rev acc) else Some (List.rev (List.rev current_group :: acc))
    | 0 :: xs ->
        if current_group = [] then aux [] acc sign xs
        else aux [] (List.rev current_group :: acc) sign xs
    | x :: xs ->
        let new_sign = if x > 0 then 1 else -1 in
        if current_group = [] then
          aux [x] acc new_sign xs
        else if new_sign = sign then
          aux (x :: current_group) acc sign xs
        else if sign = 0 then
          aux [x] acc new_sign xs
        else
          None
  in
  aux [] [] 0 lst