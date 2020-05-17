(* parse a cnf file using dolmen, but we use our representation *)
open Dolmen;;
open Cnf;;
open Cnf3;;
module M = Dimacs.Make(ParseLocation)(Term)(Statement);;

let parse_cnf_file f = 
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


let parse_cnf3_file f = 
  let sname (s: Id.t) = int_of_string @@ String.sub s.name 1 @@ (String.length s.name) - 1 in
  let rec symbol_to_var t = match t with 
  | Term.Symbol(s) -> Var (sname s)
  | Term.App (f, s) -> match symbol_to_var (List.hd s).term with | Var (i) -> NVar (i)
  in
  let rec ts_parse ts = match ts with 
  | (t: Term.t)::(t': Term.t)::(t'': Term.t)::[] ->
    (symbol_to_var t.term),(symbol_to_var t'.term),(symbol_to_var t''.term)
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