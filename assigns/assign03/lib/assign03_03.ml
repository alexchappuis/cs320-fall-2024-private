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

    