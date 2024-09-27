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

let rec collect_terminals t =
  match t with
  | Leaf _ -> [t]  (* leaf is terminal *)
  | Node [] -> [t] (* node with no children is terminal *)
  | Node children -> List.concat_map collect_terminals children

let rec collapse h t =
  if h <= 1 then
    match t with
    | Leaf _ -> t
    | Node children -> Node (List.concat_map collect_terminals children)
  else
    (* Recursive case *)
    match t with
    | Leaf _ -> t 
    | Node children ->
      Node (List.map (collapse (h - 1)) children)