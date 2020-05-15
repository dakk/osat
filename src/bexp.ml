open Dolmen;;

module M = Dimacs.Make(ParseLocation)(Term)(Statement);;

type t = 
| And of t * t 
| Or of t * t
| Not of t 
| Const of bool
| Var of int
;;

(* parse a cnf file using dolmen, but we use our representation *)
let parse_file f = 
  let rec symbol_to_var t = match t with 
  | Term.Symbol(s) -> Var (int_of_string @@ String.sub s.name 1 @@ (String.length s.name) - 1)
  | Term.App (f, s) -> Not (symbol_to_var (List.hd s).term)
  in
  let rec ts_parse ts = match ts with 
  | (t: Term.t)::[] -> symbol_to_var t.term
  | (t: Term.t)::t'::[] -> (match t.term with 
    | Term.Symbol (s) -> Or (symbol_to_var t.term, symbol_to_var t'.term)
    | Term.App (f, s) -> Or (Not (symbol_to_var (List.hd s).term), symbol_to_var t'.term)
  )
  | (t: Term.t)::t'::ts' -> (match t.term with 
    | Term.Symbol (s) -> Or (symbol_to_var t.term, ts_parse (t'::ts'))
    | Term.App (f, s) -> Or (Not (symbol_to_var (List.hd s).term), ts_parse (t'::ts'))
  )
  in
  let (sp, cl) = M.parse_input (`File f) in
  let rec ite () = match sp () with
  | None -> []
  | Some(s) -> (match s.descr with 
    | Statement.Clause(ts) -> (ts_parse ts)::(ite ())
    | s -> ite ())
  in 
  let rec andize sl = match sl with
  | c::c'::[] -> And(c, c')
  | c::cl -> And (c, andize cl)
  in
  andize @@ ite()
;;


let rec to_string l = match l with
| And (t, t') -> Printf.sprintf "(%s && %s)" (to_string t) (to_string t')
| Or (t, t') -> Printf.sprintf "(%s || %s)" (to_string t) (to_string t')
| Not (t) -> Printf.sprintf "!%s" (to_string t)
| Const (b) -> if b then "t" else "f"
| Var (v) -> Printf.sprintf "%d" v
;;

let (<|>) a b = match a, b with 
| Some(t), _ -> Some(t)
| None, Some(t) -> Some(t)
| _, _ -> None
;;

(* return first free variable *)
let rec fv b = match b with
| And (t, t') -> fv t <|> fv t'
| Or (t, t') -> fv t <|> fv t'
| Not (t) -> fv t
| Const (b) -> None
| Var (v) -> Some (v)
;;

(* replace var with value v *)
let rec repl b var v = match b with 
| And (t, t') -> And (repl t var v, repl t' var v)
| Or (t, t') -> Or (repl t var v, repl t' var v)
| Not (t) -> Not (repl t var v)
| Const (b) -> Const (b)
| Var (n) -> if n = var then Const (v) else Var (n)
;;

(* simplify an expr *)
let rec simpl b = match b with 
| And (t, t') -> 
  let st = simpl t in
  let st' = simpl t' in
  (match st, st' with
  | Const(v), Const(v') -> Const (v && v')
  | _, _ -> And (st, st'))
| Or (t, t') -> 
  let st = simpl t in
  let st' = simpl t' in
  (match st, st' with
  | Const(v), Const(v') -> Const (v || v')
  | _, _ -> Or (st, st'))
| Not (t) -> (match simpl t with
  | Const(v) -> Const(not v)
  | st -> st)
| cov -> cov
;;

(* actualize value *)
let unconst b = match b with 
| Const (b) -> b
| _ -> failwith "not a const"
;;

(* remove negation of expresion expect for var / const *)
let rec remneg b = match b with 
| Not (Not (b')) -> remneg b'
| Not (And (b', b'')) -> Or (remneg (Not b'), remneg (Not b''))
| Not (Or (b', b'')) -> And (remneg (Not b'), remneg (Not b''))
| Not (Const b') -> Const (not b')
| Not b' -> Not (remneg b')
| And (b', b'') -> And (remneg b', remneg b'')
| Or (b', b'') -> Or (remneg b', remneg b'')
| _ -> b
;;


(* distribute or over and *)
(* For example, A or (B and C) becomes (A or B) and (A or C). *)
let rec dist b = match b with 
| Or (And (y, z), x)
| Or (x, And (y, z)) -> And (Or (dist x, dist y), Or (dist x, dist z))
| Or (x, y) -> Or (dist x, dist y)
| And (x, y) -> And (dist x, dist y)
| Not (x) -> Not (dist x)
| _ -> b
;;

let rec cnf b =
  let b' = dist b |> remneg in
  if b' = b then b else cnf b'
;;