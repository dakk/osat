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
    in (re a, re b, re c)
  ;;

  let is_unit c = match c with
  | Const b, Const b', Var v
  | Const b, Var v, Const b'
  | Var v, Const b, Const b' -> Some(v, true)
  | Const b, Const b', NVar v
  | Const b, NVar v, Const b'
  | NVar v, Const b, Const b' -> Some(v, false)
  | _, _, _ -> None
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


let rec pp c = List.fold_left (fun acc x -> sprintf "%s\n%s" acc (Clause3.pp x));;
  

(* replace from cnf c literal l with v *)
let rec repl c l v = List.map (fun x -> Clause3.repl x l v) c;;

(* replace multiple literals*)
let rec repls c la = List.map (fun x -> 
  List.fold_left (fun cacc (l,v) -> 
    Clause3.repl cacc l v
  ) x la
) c;;

(* simplify a cnf *)
let rec simpl b = List.fold_left (fun acc c -> if Clause3.is_true c then acc else c::acc) [] b;;

(* return first free variable *)
let rec fv b = 
  let rec fvi c = match c with
  | NVar f, _, _
  | Var f, _, _
  | _, NVar f, _
  | _, Var f, _ 
  | _, _, NVar f
  | _, _, Var f -> Some(f)
  | _, _, _ -> None
in match b with
| [] -> None
| x :: b' -> (match fvi x with 
  | None -> fv b'
  | Some(f) -> Some(f)
);;

(* return most present literal *)
let mpv b =
  let vtbl = Hashtbl.create 16 in
  let pushv v = match Hashtbl.find_opt vtbl v with 
  | None -> Hashtbl.add vtbl v 1 
  | Some(n) -> Hashtbl.replace vtbl v (n+1)
  in
  let rec iter c = match c with 
  | [] -> 
    if Hashtbl.length vtbl = 0 then 
      None 
    else 
      Some(fst (Hashtbl.fold (fun v n acc -> if n > snd acc then (v,n) else acc) vtbl (0, 0)))
  | x::c' -> (
    (match x with
    | NVar f, _, _
    | Var f, _, _ -> pushv f
    | _, _, _ -> ());
    (match x with 
    | _, NVar f, _
    | _, Var f, _ -> pushv f
    | _, _, _ -> ());
    (match x with
    | _, _, NVar f
    | _, _, Var f -> pushv f
    | _, _, _ -> ());
    iter c'
  ) in iter b
;;

(* literal selector wrapper *)
let lit_select = mpv;;

(* returns true if all clause are true, false otherwise *)
let unconst b = 
  let unc l = 
    if Clause3.is_false l then 
      false 
    else if Clause3.is_true l then 
      true 
    else
      failwith "not a const"
  in
  let rec solve_cls cl = match cl with 
  | [] -> true
  | false::cl' -> false
  | true::cl' -> solve_cls cl'
  in solve_cls @@ List.map unc b
;;

(* remove variable with same polarity *)
let pure_polarity_removal b = 
  let ptbl = Hashtbl.create 4 in
  let push_pol x p = match Hashtbl.find_opt ptbl x with 
  | None -> Hashtbl.add ptbl x p 
  | Some (v) -> if v <> p then Hashtbl.replace ptbl x `M else ()
  in
  List.iter (fun (a,b,c) -> 
    let intern' v = match v with 
    | Var x -> push_pol x `P
    | NVar x -> push_pol x `N
    | _ -> ()
    in intern' a; intern' b;intern' c  
  ) b;
  Hashtbl.fold (fun x p (vacc, bacc) -> 
    if p = `M then  vacc, bacc
    else            (x,p = `P)::vacc, repl bacc x (p = `P)
  ) ptbl ([], b)
;;

(* let pure_polarity_removal b = 
  let ptbl = Array.make 125 `O in
  let push_pol x p = 
    let ca = Array.get ptbl x in
    if ca = `O then Array.set ptbl x p else 
    if ca <> p then Array.set ptbl x `M else ()
  in
  List.iter (fun (a,b,c) -> 
    let intern' v = match v with 
    | Var x -> push_pol x `P
    | NVar x -> push_pol x `N
    | _ -> ()
    in intern' a; intern' b;intern' c  
  ) b;
  let ll = Array.length ptbl in
  let rec ite b vacc i = 
    if i >= ll then (vacc, b) else (
    let el = Array.get ptbl i in
    if el = `P then ite (repl b i true) ((i,true)::vacc) (i+1)
    else if el = `N then ite (repl b i true) ((i,false)::vacc) (i+1)
    else ite b vacc (i+1)
  ) in ite b [] 0
;; *)


let unit_propagation b = 
  let units = List.fold_left (fun acc c -> match Clause3.is_unit c with
  | None -> acc
  | Some(v, bb) -> (v,bb)::acc
  ) [] b in units, repls b units
;;