(*let gen_fib l k =
  let rec gen_fib_helper l k =
    let len_l = List.length l in
    if k < len_l then List.nth l k
    else
      let rec sum_previous n acc count =
        if count = 0 then acc
        else sum_previous n (acc + (gen_fib_helper l (n - count))) (count - 1)
      in
      sum_previous k 0 len_l
  in
  if List.length l = 0 || k < 0 then
    failwith "not valid"
  else
    gen_fib_helper l k
*)

    let gen_fib l k = 1
 