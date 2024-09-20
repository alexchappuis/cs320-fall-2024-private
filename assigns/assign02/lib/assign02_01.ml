type piece = 
| X
| O

type pos = 
| Piece of piece
| Blank

type board = (pos * pos * pos) * (pos * pos * pos) * (pos * pos * pos)

type row_index = 
| Top
| Middle
| Bottom


type col_index = 
| Left
| Middle
| Right

type pos_index = row_index * col_index


let get_pos (b : board) ((row, col) : pos_index) : pos =
  let (top, middle, bottom) = b in
  let row_tuple = match row with
    | Top -> top
    | Middle -> middle
    | Bottom -> bottom
  in
  match col with
    | Left -> let (p, _, _) = row_tuple in p
    | Middle -> let (_, p, _) = row_tuple in p
    | Right -> let (_, _, p) = row_tuple in p


let three_in_a_row (p1 : pos) (p2 : pos) (p3 : pos) : bool =
  match p1, p2, p3 with
  | Piece X, Piece X, Piece X -> true
  | Piece O, Piece O, Piece O -> true
  | _ -> false
              
let winner (b : board) : bool =
  let ((t1, t2, t3), (m1, m2, m3), (b1, b2, b3)) = b in
  let row_win = three_in_a_row t1 t2 t3 || three_in_a_row m1 m2 m3 || three_in_a_row b1 b2 b3 in
  (* check columns *)
  let col_win = three_in_a_row t1 m1 b1 || three_in_a_row t2 m2 b2 || three_in_a_row t3 m3 b3 in
  (* check diagonals *)
  let diag_win = three_in_a_row t1 m2 b3 || three_in_a_row t3 m2 b1 in
  (* if any of the above is true there is a winner *)
  row_win || col_win || diag_win