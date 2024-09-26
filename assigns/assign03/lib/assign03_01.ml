let mk_unique_keys alst =
  let keep_track tuple (key, value) =
    let current_value = try List.assoc key tuple with 
    Not_found -> 0 in
    (key, current_value + value) :: List.remove_assoc key tuple
  in
  List.fold_left keep_track [] alst