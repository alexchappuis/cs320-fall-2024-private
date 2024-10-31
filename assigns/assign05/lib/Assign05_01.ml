type 'a test = 
  | TestCase of 'a
  | TestList of 'a test list

let rec fold_left op base test_suite =
  match test_suite with
  | TestCase x -> op base x
  | TestList tests -> List.fold_left (fold_left op) base tests