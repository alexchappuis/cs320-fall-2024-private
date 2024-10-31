type set_info = {
  ind : int -> bool; 
  mn : int;          
  mx : int;           
}

module ListSet = struct
  type t = int list

  let empty = []

  let singleton x = [x]

  let card s = List.length s

  let is_empty s = (s = [])

  let rec add x s = 
    match s with
    | [] -> [x]
    | h :: t -> 
      if x = h then s 
      else if x < h then x :: s  
      else h :: (add x t) 

  let rec mem x s =
    match s with
    | [] -> false
    | h :: t -> 
      if x = h then true
      else if x < h then false
      else mem x t

  let rec union s1 s2 =
    match s1, s2 with
    | [], s | s, [] -> s
    | h1 :: t1, h2 :: t2 ->
      if h1 = h2 then h1 :: union t1 t2
      else if h1 < h2 then h1 :: union t1 s2
      else h2 :: union s1 t2

  let rec inter s1 s2 =
    match s1, s2 with
    | [], _ | _, [] -> []
    | h1 :: t1, h2 :: t2 ->
      if h1 = h2 then h1 :: inter t1 t2
      else if h1 < h2 then inter t1 s2
      else inter s1 t2

  let rec diff s1 s2 =
    match s1, s2 with
    | [], _ -> []
    | s, [] -> s
    | h1 :: t1, h2 :: t2 ->
      if h1 = h2 then diff t1 t2
      else if h1 < h2 then h1 :: diff t1 s2
      else diff s1 t2
end

module FuncSet = struct
  type t = set_info

  let is_empty s = s.mn > s.mx
  
  let empty = { ind = (fun _ -> false); mn = 1; mx = 0 }

  let singleton x = { ind = (fun y -> x = y); mn = x; mx = x }

  let card s =
    if is_empty s then 0
    else
      let count = ref 0 in
      for i = s.mn to s.mx do
        if s.ind i then incr count
      done;
      !count

  let add x s =
    let new_ind y = s.ind y || x = y in
    if is_empty s then { ind = new_ind; mn = x; mx = x }
    else { ind = new_ind; mn = min x s.mn; mx = max x s.mx }

  let mem x s = s.ind x

  let union s1 s2 =
    if is_empty s1 then s2
    else if is_empty s2 then s1
    else {
      ind = (fun x -> s1.ind x || s2.ind x);
      mn = min s1.mn s2.mn;
      mx = max s1.mx s2.mx;
    }

    let inter s1 s2 =
      if is_empty s1 || is_empty s2 then empty
      else {
        ind = (fun x -> s1.ind x && s2.ind x);
        mn = max s1.mn s2.mn;
        mx = min s1.mx s2.mx;
      }
  
    let diff s1 s2 =
      if is_empty s1 then empty
      else if is_empty s2 then s1
      else {
        ind = (fun x -> s1.ind x && not (s2.ind x));
        mn = s1.mn;
        mx = s1.mx; 
      }
  end
  