(* simple backtracking *)

open Bexp;;

let rec satisfiable b = match fv b with
| None -> unconst b 
| Some (v) -> 
  let tg = simpl (repl b v true) in
  let fg = simpl (repl b v false) in
  satisfiable tg || satisfiable fg
;;

let rec solve b = match fv b with 
| None -> []
| Some (v) ->
  let tg = simpl (repl b v true) in
  let fg = simpl (repl b v false) in
  if satisfiable tg then (v,true)::(solve tg)
  else if satisfiable fg then (v,false)::(solve fg)
  else failwith "No solution available"
;;
