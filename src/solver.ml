open Bexp;;

type sol = (int * bool) list;;

module type t = sig
  val satisfiable : Bexp.t -> bool
  val solve       : Bexp.t -> sol
end


let rec print_sol s = match s with 
| [] -> ()
| (v,vv)::s' -> Printf.printf "%d => %b\t%!" v vv; print_sol s'
;;

let rec verify_sol b s = match b with 
| Var x -> List.assoc x s
| And (x, x') -> (verify_sol x s) && (verify_sol x' s)
| Or (x, x') -> (verify_sol x s) || (verify_sol x' s)
| Not (x) -> not (verify_sol x s)
;;