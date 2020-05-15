open Printf;;

let solvers : (string * (module Solver.t)) list = [
  ("bt", (module Solver_bt))
];;


let main () = 
  if Array.length Sys.argv < 3 then printf "usage: %s solver file.cnf\n" Sys.argv.(0) else (
    let (module S: Solver.t) = (List.assoc Sys.argv.(1) solvers) in 
    let f = Sys.argv.(2) in
    printf "loading %s... %!" f;
    let t = Bexp.parse_file f in 
    printf "done\nrunning satisfiable (using %s solver)...\n%!" Sys.argv.(1);
    printf "satisfiable: %b\n%!" (S.satisfiable t)
  )
in main();;