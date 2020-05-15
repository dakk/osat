open Printf;;

let solvers : (string * (module Solver.t)) list = [
  ("bt", (module Solver_bt))
];;


let (module S: Solver.t) = (List.hd solvers) |> snd in
printf "%b %b\n%!" (S.satisfiable (Const true)) (S.satisfiable (Const false));
printf "%b %b\n%!" (S.satisfiable (And (Var 1, Not(Var 1)))) (S.satisfiable (Or (Var 1, Not(Var 1))));;