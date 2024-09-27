type tree = 
| Leaf of int
| Node of tree list


let t =
  Node
    [ Node
      [ Leaf 1
      ; Node []
      ; Leaf 100
      ]
    ; Leaf 1
    ]

let rec get_terminals t =
  match t with
  | Leaf _ -> [t]  (* terminal *)
  | Node [] -> [t] 
  | Node children -> List.concat_map get_terminals children

let rec collapse h t =
  if h <= 1 then
    match t with
    | Leaf _ -> t
    | Node children -> Node (List.concat_map get_terminals children)
  else
    match t with
    | Leaf _ -> t 
    | Node children ->
      Node (List.map (collapse (h - 1)) children)