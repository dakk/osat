open Cnf3;;

module Bt = struct
  let rec satisfiable b = match fv b with
  | None -> unconst b 
  | Some (v) -> 
    satisfiable @@ simpl (repl b v true) 
    || 
    satisfiable @@ simpl (repl b v false)
  ;;

  let rec solve b = match fv b with 
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
    (* let s', b = Cnf3.same_polarity_removal b in
    Printf.printf "S %d replaced\n%!" (List.length s'); *)
    match fv b with
  | None -> unconst b 
  | Some (v) -> 
    satisfiable @@ simpl (repl b v true) 
    || 
    satisfiable @@ simpl (repl b v false)
  ;;


  let rec solve2 b = 
    let s', b = Cnf3.unit_propagation b in
    let s'', b = Cnf3.pure_polarity_removal b in
    match fv b with 
  | None -> if unconst b then s' @ s'' @ [] else failwith "not a good solution"
  | Some (v) ->
    try 
      let tg = solve2 @@ simpl (repl b v true) in
      (v,true)::(s' @ s'' @ tg)
    with | _ ->  try 
      let fg = solve2 @@ simpl (repl b v false) in
      (v,false)::(s' @ s'' @ fg)
    with | _ -> 
      failwith "no solution"
  ;;

  let rec solve b = 
    let s', b = Cnf3.unit_propagation b in
    let s'', b = Cnf3.pure_polarity_removal b in
    match fv b with 
  | None -> []
  | Some (v) ->
    let tg = simpl (repl b v true) in
    if satisfiable tg then (
      (v,true)::(s' @ s'' @ solve tg)
    ) else (
      let fg = simpl (repl b v false) in
      if satisfiable fg then 
        (v,false)::(s' @ s'' @ solve fg)
      else 
        failwith "no solution available"
    )
  ;;
end

let rec print s = match s with 
| [] -> ()
| (v,vv)::s' -> Printf.printf "%d => %b\t%!" v vv; print s'
;;

let rec verify b s = 
  let rec replit b' s = match s with  
  | [] -> b'
  | (v,vv)::s' -> replit (repl b' v vv) s'
  in 
  unconst @@ simpl @@ replit b s
;;
