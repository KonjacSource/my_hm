open Environment
open Syntax

let generalize (env : Env.t) (t : ty) : Scheme.t = 
  Forall (exclude (Type.fv t) (Env.fv env), t)

let var_counter : int ref = ref 0

let freshVar : ty = 
  let count = ! var_counter in 
  var_counter := count + 1;
    TyVar ("a" ^ Int.to_string count)

(* Kind like inserting new meta variables *)
let instantiate (Forall (vars, t) : Scheme.t) : ty = 
  let insert_vars = NameMap.of_seq @@ List.(
    to_seq (map (fun ori -> (ori, freshVar)) (NameSet.elements vars))
  ) in 
  Type.subst insert_vars t

type check_error = 
  | UnifyError of ty * ty 
  | VariableOccur of name * ty
  | UnknownVar of name

exception CheckError of check_error 

let bind_var (x : name) (t : ty) : substor = 
  match t with 
  | TyVar x' when x = x' -> null
  | _        when NameSet.mem x (Type.fv t) -> raise (CheckError (VariableOccur (x, t)))
  | _        -> NameMap.singleton x t

let rec unify (t1 : ty) (t2 : ty) : substor = 
  let open Type in
  match t1, t2 with 
  | Func(a1, b1), Func(a2, b2) -> 
      let s1 = unify a1 a2 in let
          s2 = unify (subst s1 b1) (subst s1 b2) in 
      s1 #+ s2
  | TyVar x, t | t, TyVar x-> bind_var x t 
  | UnitTy, UnitTy | NatTy, NatTy -> null
  | _ -> raise (CheckError (UnifyError (t1,t2)))

let rec infer (env : Env.t) (e : term) : substor * ty = 
  let open NameMap in 
  let open Scheme in
    match e with
    | Var x -> (match find_opt x env with 
                | Some s -> null, instantiate s
                | _ -> raise (CheckError (UnknownVar x)))

    | UnitLit -> null, UnitTy

    | NatLit _ -> null, NatTy

    | App (f, a) -> 
        let tfa    = freshVar in 
        let s1, tf = infer env f in 
        let s2, ta = infer (Env.subst s1 env) a in 
        let s3     = unify (Type.subst s2 tf) (Func (ta, tfa)) in 
        s3 #+ s2 #+ s1, Type.subst s3 tfa

    | Lam (x, body) -> 
        let tx    = freshVar in 
        let env'  = add x (Forall (NameSet.empty, tx)) env in 
        let s1,t1 = infer env' body in
        s1, Func (Type.subst s1 tx, t1)

    | Let (x, v, body) -> 
        let s1, tv     = infer env v in 
        let tx         = generalize (Env.subst s1 env) tv in
        let env'       = add x tx env in 
        let s2, t_body = infer (Env.subst s1 env') body in 
        s1 #+ s2, t_body 

    | Predef _ -> undefined ()


let inference (env : Env.t) (e : term) : ty = 
  let s, t =  infer env e in Type.subst s t