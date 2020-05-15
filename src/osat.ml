open Printf;;

let solvers : (string * (module Solver.t)) list = [
  ("bt", (module Solver_bt))
];;

(* open Bexp;;

let (module S: Solver.t) = (List.assoc "bt" solvers) in 
let t = And (Or (Var 1, Var 2), Or (Var 1, Not (Var 2))) in

printf "%s\n" (to_string @@ lit_elim t); *)
(* let s = S.solve t in
Solver.print_sol s;
printf "verify sol: %b\n" @@ Solver.verify_sol t s;
let t = Or(Not (Var 1), Not (Var 2)) in
let s = S.solve t in
Solver.print_sol s;
printf "verify sol: %b\n" @@ Solver.verify_sol t s;
let t = And(Not (Var 1), Not (Var 1)) in
let s = S.solve t in
Solver.print_sol s;
printf "verify sol: %b\n" @@ Solver.verify_sol t s;; *)


let main () = 
  if Array.length Sys.argv < 3 then printf "usage: %s solver file.cnf\n" Sys.argv.(0) else (
    let (module S: Solver.t) = (List.assoc Sys.argv.(1) solvers) in 
    let f = Sys.argv.(2) in
    printf "loading %s... %!" f;
    let t = Bexp.parse_file f in 
    printf "done\nrunning solver (%s)...\n%!" Sys.argv.(1);
    (* let satisf = (S.satisfiable t) in
    printf "satisfiable: %b\n%!" satisf;
    if satisf then ( *)
    try 
      let t = Bexp.simpl @@ Bexp.lit_elim t in
      (* @@ Bexp.cnf t in *)
      (* let a = Unix.time () in *)
      let s = S.solve t in
      (* let b = Unix.time () -. a in *)
      printf "solution found:\n%!";
      Solver.print_sol s;
      (* printf "found in %f seconds\n" b *)
      printf "verify sol: %b\n" (Solver.verify_sol t s)
    with | _ -> printf "no solution found\n%!";
  )
in main();;