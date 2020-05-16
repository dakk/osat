open Printf;;

(* since every cnf can be translated to 3cnf, we use only this type*)
type lit = Var of int | NVar of int | Const of bool;;
type clause3 = lit * lit * lit;;
type cnf3 = clause3 list;;

let trd (a,b,c) = c;;

module Clause3 = struct
  let repl (a,b,c) l v = 
    let re el = match el with 
    | Var (x) when x = l -> Const v
    | NVar (x) when x = l -> Const (not v)
    | _ -> el
    in 
    (re a, re b, re c)
  ;;

  let is be (a,b,c)=
    let has el = match el with | Const be -> true | _ -> false in
    if be then has a || has b || has c else has a && has b && has c
  ;;

  let is_true (a,b,c) = 
    let has el = match el with | Const true -> true | _ -> false in
    has a || has b || has c
  ;;

  let is_false (a,b,c) = 
    let has el = match el with | Const false -> true | _ -> false in
    has a && has b && has c
  ;;

  let to_list (a,b,c) = a::b::c::[];;

  let pp_lit l = match l with
  | Var (x) -> sprintf "%d" x
  | NVar (x) -> sprintf "!%d" x
  | Const (b) -> sprintf "%b" b
  ;;

  let pp (a,b,c) = sprintf "%s || %s || %s" (pp_lit a) (pp_lit b) (pp_lit c);;
end


let rec pp_cnf c = match c with
| [] -> "\n"
| c'::cl' -> sprintf "%s\n%s" (Clause3.pp c') (pp_cnf cl')
;;


(* replace from cnf c literal l with v *)
let rec repl c l v = match c with
| [] -> []
| x::c' -> (Clause3.repl x l v)::(repl c' l v)
;;

(* simplify a cnf *)
let rec simpl b = 
  List.fold_left (fun acc c -> 
    if Clause3.is_true c then acc else c::acc
  ) [] b
;;

(* return first free variable *)
let rec fv b = 
  let rec fvi c = match c with
  | NVar f, _, _
  | Var f, _, _ -> Some(f)
  | _, NVar f, _
  | _, Var f, _ -> Some(f)
  | _, _, NVar f
  | _, _, Var f -> Some(f)
  | _, _, _ -> None
in match b with
| [] -> None
| x :: b' -> (match fvi x with 
  | None -> fv b'
  | Some(f) -> Some(f)
);;


(* an optimization could be selecting as first fv the most used; this will cause a simplification *)
(* let rec mffv b *)

let unconst b = 
  let unc l = 
    if Clause3.is_false l then false else if Clause3.is_true l then true else (
      printf "%s\n%!" (Clause3.pp l);
      failwith "not a const"
    )
  in
  let b' = List.map unc b in
  let rec solve_cls cl = match cl with 
  | [] -> true
  | false::cl' -> false
  | true::cl' -> solve_cls cl'
  in solve_cls b'
;;

(* remove variable with same polarity *)
let pure_polarity_removal b = 
  let push_pol pl x p = 
    if List.mem_assoc x pl then (
      if (List.assoc x pl) = p then pl 
      else (x,`M)::(List.remove_assoc x pl)
    ) else
      (x,p)::pl
  in
  let rec cl_pol c pl = match c with 
  | [] -> pl
  | (Var x)::xl' -> cl_pol xl' (push_pol pl x `P)
  | (NVar x)::xl' -> cl_pol xl' (push_pol pl x `N)
  | _::xl' -> cl_pol xl' pl
  in
  let rec pol c pl = match c with
  | [] -> pl 
  | x::xl' -> pol xl' (cl_pol (Clause3.to_list x) pl)
  in 
  let vars = pol b [] |> List.filter (fun (x,p) -> p <> `M) |> List.map (fun (x,p) -> (x,p = `P)) in
  let rec repl_pol b' pol = match pol with
  | [] -> b'
  | (x,v)::p' -> repl_pol (repl b' x v) p'
  in
  (vars, repl_pol b vars)
;;


let unit_propagation b = [], b;;