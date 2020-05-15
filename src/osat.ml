open Printf;;

let solvers : (string * (module Solver.t)) list = [
  ("bt", (module Solver_bt))
];;


let (module S: Solver.t) = (List.hd solvers) |> snd in
(* printf "%b %b\n%!" (S.satisfiable (Const true)) (S.satisfiable (Const false));
printf "%b %b\n%!" (S.satisfiable (And (Var 1, Not(Var 1)))) (S.satisfiable (Or (Var 1, Not(Var 1)))); *)


let t = Bexp.parse_file "./test/bf.cnf" in 
printf "%s\n\nsatisfiable: %b\n%!" (Bexp.to_string t) (S.satisfiable t);