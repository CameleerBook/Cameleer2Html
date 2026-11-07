let error m =
  Printf.eprintf "error: %s\n" m;
  exit 1

let info s m =
  Printf.printf s m

let format s p =
  Printf.sprintf s p

let write c s =
  output_string c s