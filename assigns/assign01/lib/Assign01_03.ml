open Assign01_02

let nth s i = 
  let prime = nth_prime i in
    let rec count_exponent encoded count =
    if encoded mod prime <> 0 then 
      count
    else 
      count_exponent (encoded / prime) (count + 1)
  
in count_exponent s 0