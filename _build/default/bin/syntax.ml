
module NameMap = Map.Make(String);;
module NameSet = Set.Make(String);;

type name = string

(* IndN : (unit -> a) -> (nat -> a -> a) -> a *)
type funs = Suc | Zero | IndN

type term = 
| UnitLit
| NatLit of int 
| App of term * term
| Let of name * term * term 
| Lam of name * term 
| Predef of funs

type ty = 
| UnitTy 
| NatTy 
| Func of ty * ty 
| TyVar of name

type scheme = Forall of NameSet.t * ty  

type substor = ty NameMap.t

let rec subst_type (sub: substor) (t: ty): ty = 
  match t with
  | Func (a, b) -> Func (subst_type sub a, subst_type sub b)
  | TyVar x -> (match NameMap.find_opt x sub with 
    | None -> TyVar x 
    | Some t -> t)
  | x -> x

module type Types = sig 
  type t
  val fv : t -> NameSet.t
  val subst : substor -> t -> t
end

module Type : Types = struct 
  type t = ty 
  let rec fv (t : ty) = match t with 
    | TyVar x -> NameSet.singleton x 
    | Func (a, b) -> NameSet.union (fv a) (fv b)
    | _ -> NameSet.empty
  let subst = subst_type
end

(* constraint of two type, we say they are equal. *)
type type_equation = Eq of ty * ty

