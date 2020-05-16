open Printf;;

(* let solvers : (string * (module Solver.t)) list = [
  ("bt", (module Solver_bt))
];; *)

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
    (* let (module S: Solver.t) = (List.assoc Sys.argv.(1) solvers) in  *)
    let f = Sys.argv.(2) in
    printf "loading %s... %!" f;
    let t = Bexp2.parse_file f in 

    printf "done\noptimizing... %!";
    let s', t' = Bexp2.same_polarity_removal t in
    (* let s' = [] in
    let s', t' = Bexp.lit_elim @@ Bexp.remneg t in
    let t' = Bexp.simpl t' in *)
    (* @@ Bexp.cnf t in *)
    printf "done\nrunning solver (%s)...\n%!" Sys.argv.(1);
    (* let satisf = (S.satisfiable t) in
    printf "satisfiable: %b\n%!" satisf;
    if satisf then ( *)
    (* try  *)
      (* let a = Unix.time () in *)
      let s = Bexp2.solve t' @ s' in
      (* let b = Unix.time () -. a in *)
      printf "solution found:\n%!";
      Bexp2.print_sol s;
      (* printf "found in %f seconds\n" b *)
      printf "\nverify sol: %b\n" (Bexp2.verify_sol t s)
    (* with | _ -> printf "no solution found\n%!"; *)
  )
in main();;