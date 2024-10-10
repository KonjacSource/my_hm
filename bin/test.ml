open Syntax
let e1 = 
  Let ("id", Lam ("x", Var "x")
  , Var "id"
  )

let tryInfer = Infer.inference NameMap.empty