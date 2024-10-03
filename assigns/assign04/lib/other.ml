let last_function_standing (funcs : ('a -> 'a) list) (start : 'a) (pred : 'a -> bool) : ('a -> 'a) option =
  let lifespan f s =
    let rec aux i current =
      if pred current then i - 1
      else aux (i + 1) (f current)
    in
    aux 1 s
  in

  match funcs with
  | [] -> None
  | _ ->
    let results = List.map (fun f -> (f, lifespan f start)) funcs in
    let max_lifespan = List.fold_left (fun acc (_, lifespan) ->
      match acc with
      | None -> Some lifespan
      | Some max_l when lifespan > max_l -> Some lifespan
      | Some max_l when lifespan = max_l -> None
      | Some max_l -> Some max_l) None results
    in
    match max_lifespan with
    | None -> None
    | Some max_value ->
      let filtered = List.filter (fun (_, l) -> l = max_value) results in
      match filtered with
      | [(f, _)] -> Some f
      | _ -> None
