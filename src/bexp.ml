open Printf;;

type t = 
| And of t * t 
| Or of t * t
| Not of t 
| Const of bool
| Var of int
;;


let rec pp l = match l with
| And (t, t') -> sprintf "(%s && %s)" (pp t) (pp t')
| Or (t, t') -> sprintf "(%s || %s)" (pp t) (pp t')
| Not (t) -> sprintf "!%s" (pp t)
| Const (b) -> if b then "t" else "f"
| Var (v) -> sprintf "%d" v
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


(* transform to cnf *)
let rec cnf b =
  let b' = dist b |> remneg in
  if b' = b then b else cnf b'
;;
