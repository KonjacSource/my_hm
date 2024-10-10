
module NameMap = Map.Make(String);;
module NameSet = Set.Make(String);;


exception Undefined
let undefined () = raise Undefined

type name = string

let exclude (a : NameSet.t) (b : NameSet.t) : NameSet.t = 
  List.fold_right (fun e acc -> NameSet.remove e acc) (NameSet.elements b) a

(* IndN : (unit -> a) -> (nat -> a -> a) -> a *)
type funs = Suc | Zero | IndN

type term = 
| UnitLit
| NatLit of int 
| App of term * term
| Let of name * term * term 
| Lam of name * term
| Var of name 
| Predef of funs

type ty = 
| UnitTy 
| NatTy 
| Func of ty * ty 
| TyVar of name

type substor = ty NameMap.t

let rec subst_type (sub: substor) (t: ty): ty = 
  match t with
  | Func (a, b) -> Func (subst_type sub a, subst_type sub b)
  | TyVar x -> (match NameMap.find_opt x sub with 
    | None -> TyVar x 
    | Some t -> t)
  | x -> x

let null : substor = NameMap.empty

(* Use s2 then s1 *)
let (#+) (s1 : substor) (s2 : substor) : substor = 
  NameMap.( union (fun _ x _ -> Some x) (map (subst_type s1) s2) s1 )

module Type = struct 
  type t = ty 
  let rec fv (t : ty) = match t with 
    | TyVar x -> NameSet.singleton x 
    | Func (a, b) -> NameSet.union (fv a) (fv b)
    | _ -> NameSet.empty
  let subst = subst_type

  let rec to_string' (top : bool) (t : ty) : string = 
    let paren x = if top then x else "(" ^ x ^ ")" in
    match t with 
    | UnitTy -> "()"
    | NatTy -> "Nat"
    | TyVar x -> x 
    | Func(a,b) -> paren @@ to_string' false a ^ " -> " ^ to_string' true b
  let to_string = to_string' true

end

module Scheme = struct 
  type t = Forall of NameSet.t * ty  
  let fv = 
    fun (Forall (binders, body)) -> 
      exclude (Type.fv body) binders
  let subst (s : substor) (Forall (binders, body) : t) = 
    Forall 
      ( binders
      , Type.subst 
          (List.fold_right 
            (fun e acc -> NameMap.remove e acc) 
            (NameSet.elements binders) 
            s) 
          body
      )
end

module TypeList 
  (A : sig
     type t
     val fv : t -> NameSet.t 
     val subst : substor -> t -> t 
    end) = 
struct 
  open List
  type t = A.t list

  let fv (ls : t) = fold_right NameSet.union (map A.fv ls) NameSet.empty
  let subst (s : substor) (ls : t) : t = map (A.subst s) ls
      
end

(* constraint of two type, we say they are equal. *)
type type_equation = Eq of ty * ty
