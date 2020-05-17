open Cnf3;;

(* todo: transform this module as a functor over representation modules (Cnf3/Cnf/Bexp) *)

module Bt = struct
  let rec satisfiable b = match lit_select b with
  | None -> unconst b 
  | Some (v) -> 
    satisfiable @@ simpl (repl b v true) 
    || 
    satisfiable @@ simpl (repl b v false)
  ;;

  let rec solve b = match lit_select b with 
  | None -> []
  | Some (v) ->
    let tg = simpl (repl b v true) in
    if satisfiable tg then (
      (v,true)::(solve tg)
    ) else (
      let fg = simpl (repl b v false) in
      if satisfiable fg then 
        (v,false)::(solve fg)
      else 
        failwith "no solution available"
    )
  ;;
end


module DPLL = struct
  let rec satisfiable b = 
    let s', b = Cnf3.unit_propagation b in
    let s'', b = Cnf3.pure_polarity_removal b in
    match lit_select b with
  | None -> unconst b 
  | Some (v) -> 
    satisfiable @@ simpl (repl b v true) 
    || 
    satisfiable @@ simpl (repl b v false)
  ;;

  let rec solve b = 
    let s', b = Cnf3.unit_propagation b in
    let s'', b = Cnf3.pure_polarity_removal b in
    match lit_select b with 
  | None -> if unconst b then s' @ s'' @ [] else failwith "not a good solution"
  | Some (v) ->
    try 
      let tg = solve @@ simpl (repl b v true) in
      (v,true)::(s' @ s'' @ tg)
    with | _ ->  try 
      let fg = solve @@ simpl (repl b v false) in
      (v,false)::(s' @ s'' @ fg)
    with | _ -> 
      failwith "no solution"
  ;;
end

let rec print s = match s with 
| [] -> ()
| (v,vv)::s' -> Printf.printf "%d: %s\t%!" v (if vv then "t" else "f"); print s'
;;

let rec verify b s = 
  let rec replit b' s = match s with  
  | [] -> b'
  | (v,vv)::s' -> replit (repl b' v vv) s'
  in 
  unconst @@ simpl @@ replit b s
;;
