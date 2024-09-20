type dir = 
| North
| South
| East
| West

type path = dir list

let dist (dirs : path) : float =
  let rec compute_position dirs (x, y) =
    match dirs with
    | [] -> (x, y) (* no more directions *)
    | North :: rest -> compute_position rest (x, y + 1) 
    | South :: rest -> compute_position rest (x, y - 1)
    | East :: rest -> compute_position rest (x + 1, y)
    | West :: rest -> compute_position rest (x - 1, y)
  in
  let (final_x, final_y) = compute_position dirs (0, 0) in
  sqrt (float_of_int (final_x * final_x + final_y * final_y))
