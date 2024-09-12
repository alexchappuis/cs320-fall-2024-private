open Assign01_02

let nth s i = 
  let prime = nth_prime i
  in 
  let rec count encoded numPrimes = 
    if encoded mod prime <> 0 then 
      numPrimes
    else 
      count (encoded / prime) (numPrimes + 1)

  in count s 0;;
