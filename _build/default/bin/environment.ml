open Syntax

module Env = struct 
  
  type t = Scheme.t NameMap.t

  (* Remove a variable in context *)
  let remove (env : t) (name : name) : t = 
    NameMap.remove name env

  let fv (env : t) = 
    Seq.fold_left 
      (fun acc (_, t) -> NameSet.union (Scheme.fv t) acc) 
      NameSet.empty 
      (NameMap.to_seq env)
  
  let subst (s : substor) (env : t) : t = 
    NameMap.map (fun t -> Scheme.subst s t) env

end