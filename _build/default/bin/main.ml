open Syntax 
open Infer

let () = 
  print_endline "yay" ;
  let es = [
    Let ("id", Lam ("x", Var "x")
    , Var "id"
    )
  ] in 
  let ts = List.map (inference NameMap.empty) es in 
  let _ = List.map (fun t -> print_endline (Type.to_string t)) ts in 
  ()

