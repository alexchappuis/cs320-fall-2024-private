let is_prime n =
  let rec check i =
    if i * i > n then true
    else if n mod i = 0 then false
    else check (i + 1)
  in
  if n <= 1 then false
  else check 2


let nth_prime i =
  let rec find_primes count current_prime =
    if count > i then
      current_prime
    else
      let next_prime = 
        let rec find_next n =
          if is_prime n then n
          else find_next (n + 1)
        in
        find_next (current_prime + 1)
      in
      find_primes (count + 1) next_prime
  in
  find_primes 0 1