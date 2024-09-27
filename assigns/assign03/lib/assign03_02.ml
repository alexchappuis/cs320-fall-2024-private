let gen_fib l k =
  let len = List.length l in
  let rec aux n acc =
    let rec sum_of_previous i total =
      if i <= 0 then total
      else sum_of_previous (i - 1) (total + aux (n - i) acc)
    in
      let sum_of_previous i total =
        if i = 0 then total
        else sum_of_previous (i - 1) (total + aux (n - i) acc)
      in
      aux (n + 1) (sum_of_previous len 0)  (* recursive call with update parameters *)
  in
  if k < 0 || len = 0 then failwith "invalid"
  else aux k 0
