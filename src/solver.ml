open Bexp;;

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
