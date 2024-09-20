type matrix = {
  entries : float list list;
  rows : int;
  cols : int;
}

let rec partition lst size =
  match lst with
  | [] -> []
  | _ ->
      let row = List.take size lst in
      let rest = List.drop size lst in
      row :: partition rest size

let mk_matrix (entries : float list) ((r, c) : int * int) : matrix =
  let rows = partition entries c in
  { entries = rows; rows = r; cols = c }
