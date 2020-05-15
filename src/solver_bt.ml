(* simple backtracking *)

open Bexp;;

let rec satisfiable b = match fv b with
| None -> unconst b 
| Some (v) -> 
  let tg = simpl (repl b v true) in
  let fg = simpl (repl b v false) in
  satisfiable tg || satisfiable fg
;;
