open Printf;;

(* we define a new abstraction, more suitable for processing, for representing cnf *)
type lit = Var of int | NVar of int | Const of bool;;
type clause = lit list;;
type cnf = clause list;;

let pp_lit l = match l with
| Var (x) -> sprintf "%d" x
| NVar (x) -> sprintf "!%d" x
| Const (b) -> sprintf "%b" b
;;

let rec pp_clause c = match c with
| [] -> "\n"
| c'::cl' -> sprintf "%s || %s" (pp_lit c') (pp_clause cl')
;;

let rec pp_cnf c = match c with
| [] -> "\n"
| c'::cl' -> sprintf "%s\n%s" (pp_clause c') (pp_cnf cl')
;;

(* replace from cnf c literal l with v *)
let rec repl c l v = 
  let rec rc cl = match cl with
  | [] -> []
  | (Var x)::cl' when x = l -> (Const v)::(rc cl')
  | (NVar x)::cl' when x = l -> (Const (not v))::(rc cl')
  | x::cl' -> x::(rc cl')
in match c with
| [] -> []
| x::c' -> (rc x)::(repl c' l v)
;;

(* solve a clause; if has a true, is true, if has n false is false *)
let rec clause_solve cl f i acc = match cl with 
| [] -> if f = i then [Const false] else acc
| (Const true)::cl' -> [Const true]
| (Const false)::cl' -> clause_solve cl' (f+1) (i+1) acc
| c::cl' -> clause_solve cl' f (i+1) (c::acc)
;;

(* simplify a cnf *)
let rec simpl b = 
  (* we apply removal on each clause, we remove true clauses *)
  (* List.fold_left (fun acc c -> 
    let a = clause_solve c 0 0 [] in 
    if a <> [Const true] then a::acc else acc
  ) b [] *)
  List.map (fun c -> clause_solve c 0 0 []) b |> List.filter (fun c -> c <> [Const true])
;;


(* return first free variable *)
let rec fv b = 
  let rec fvi c = match c with
  | [] -> None
  | (Var f)::c' -> Some(f)
  | (NVar f)::c' -> Some(f)
  | _::c' -> fvi c'
in match b with
| [] -> None
| x :: b' -> (match fvi x with 
  | None -> fv b'
  | Some(f) -> Some(f)
);;


(* parse a cnf file using dolmen, but we use our representation *)
open Dolmen;;
module M = Dimacs.Make(ParseLocation)(Term)(Statement);;

let parse_file f = 
  let sname (s: Id.t) = int_of_string @@ String.sub s.name 1 @@ (String.length s.name) - 1 in
  let rec symbol_to_var t = match t with 
  | Term.Symbol(s) -> Var (sname s)
  | Term.App (f, s) -> match symbol_to_var (List.hd s).term with | Var (i) -> NVar (i)
  in
  let rec ts_parse ts = match ts with 
  | [] -> []
  | (t: Term.t)::ts' -> (symbol_to_var t.term)::(ts_parse ts')
  in
  let (sp, cl) = M.parse_input (`File f) in
  let rec ite () = match sp () with
  | None -> []
  | Some(s) -> (match s.descr with 
    | Statement.Clause(ts) -> (ts_parse ts)::(ite ())
    | _ -> ite ())
  in 
  let rec andize sl = match sl with
  | [] -> []
  | c::cl -> c::(andize cl)
  in
  let r = andize @@ ite() in 
  cl ();
  r
;;


let unconst b = 
  let unc l = 
    if l = [Const (false)] then 
      false 
    else if l = [Const (true)] then 
      true 
    else (
      printf "%s\n%!" (pp_clause l);
      failwith "not a const"
    )
  in
  let b' = List.map unc b in
  if List.length (List.filter (fun l -> l) b') = List.length(b') then true else false
;;

let rec satisfiable b = match fv b with
| None -> unconst b 
| Some (v) -> 
  let tg = simpl (repl b v true) in
  let fg = simpl (repl b v false) in
  satisfiable tg || satisfiable fg
;;

let rec solve b = match fv b with 
| None -> []
| Some (v) ->
  let tg = simpl (repl b v true) in
  if satisfiable tg then (
    (v,true)::(solve tg)
  ) else (
    let fg = simpl (repl b v false) in
    if satisfiable fg then 
      (v,false)::(solve fg)
    else 
      failwith "no solution available"
  )
;;


let rec print_sol s = match s with 
| [] -> ()
| (v,vv)::s' -> Printf.printf "%d => %b\t%!" v vv; print_sol s'
;;

let rec verify_sol b s = 
  let rec replit b' s = match s with  
  | [] -> b'
  | (v,vv)::s' -> replit (repl b' v vv) s'
  in 
  unconst @@ simpl @@ replit b s
;;


(* remove solutions with same polarity *)
let same_polarity_removal b = 
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
  in
  let rec pol c pl = match c with
  | [] -> pl 
  | x::xl' -> pol xl' (cl_pol x pl)
  in 
  let polarities = pol b [] |> List.filter (fun (x,p) -> p <> `M) |> List.map (fun (x,p) -> (x,p = `P)) in
  let rec repl_pol b' pol = match pol with
  | [] -> b'
  | (x,v)::p' -> 
    repl_pol (repl b' x v) p'
  in
  (polarities, repl_pol b polarities)
;;