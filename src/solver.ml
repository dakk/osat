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


module Genetic = struct 
  type chromosome = (int * bool) list;;

  let pop_size = 64;;

  let satisfiable b = failwith "not implemented";;

  let rec random_solution vl = match vl with
  | [] -> []
  | a::vl' -> (a, Random.bool ())::(random_solution vl')
  ;;

  (* calculate the fitness of the given solution (as number of true caluse vs false) *)
  let rec fitness_of b sol =
    let b' = simpl @@ repls b sol in 
    let l = List.length b in
    let l' = List.length b' in
    if unconst b' then 1.0 else 1.0 -. (float_of_int l') /. (float_of_int l)
  ;;

  let rec solve_step i b vl pop =
    let var_n = List.length vl in 
    let rec sub l n = if n = 0 then [] else match l with 
      | [] -> []
      | (s, f)::l' -> s::(sub l' (n-1))
    in  
    let crossover a b = 
      let cpoint = Random.int (var_n - 1) in 
      let rec ap (a, b) (ac', bc') n = match a, b, n with 
      | [], [], n -> ac', bc'
      | ae::a', be::b', n when n <= cpoint -> ap (a', b') (be::ac', ae::bc') (n-1)
      | ae::a', be::b', n when n > cpoint -> ap (a', b') (ae::ac', be::bc') (n-1)
      in ap (a, b) ([], []) 0
    in
    let rec apply_cross l = match l with 
    | [] -> []
    | x::[] -> let a, b = crossover x x in a::b::[]
    | x::x'::l' -> let a, b = crossover x x' in a::b::(apply_cross l')
    in
    let mutate t = 
      let rec mut t n = if n = 0 then t else (
        let cpoint = Random.int var_n in
        mut (List.mapi (fun i (v,x) -> if i = cpoint then (v, not x) else (v, x)) t) (n-1)
      )
      in
      mut t (Random.int @@ var_n / 8)
    in
    let rec shuffle = function
    | [] -> []
    | [single] -> [single]
    | list -> 
      let (before, after) = List.partition (fun elt -> Random.bool ()) list in 
      List.rev_append (shuffle before) (shuffle after)
    in
    if snd (List.hd pop) = 1.0 then fst (List.hd pop) else (
      let avgf = (List.fold_left (fun acc x -> acc +. (snd x)) 0.0 pop) /. (float_of_int @@ pop_size + 1) in
      Printf.printf "[%d] best has a fitness of %f (%f avg, %d cr)\n%!" i (snd (List.hd pop)) avgf (pop_size + 1);

      (* select best *)
      let best = sub pop (pop_size / 2) |> shuffle in

      (* crossover *)
      let crossed = apply_cross best |> List.map mutate in

      (* create new pop *)
      let pop' = crossed @ best @ [random_solution vl] in
      (* let pop' = pop' |> List.map (fun t -> if (Random.int 100 < 50) then mutate t else t) in *)

      (* sort new pop *)
      let pop' = List.sort (fun a b -> compare (snd b) (snd a)) @@ List.map (fun s -> (s, fitness_of b s) ) pop' in
      solve_step (i+1) b vl pop'
    )
  ;;

  let solve b = 
    Random.self_init ();
    let vl = vars b in

    (* we first create a random population where each chromosome is a random solution with fitness *)
    let rec gen_pop n = if n = 0 then [] else (random_solution vl)::(gen_pop (n-1)) in
    let pop = List.sort (fun a b -> compare (snd b) (snd a)) @@ List.map (fun s -> (s, fitness_of b s) ) @@ gen_pop pop_size in

    solve_step 1 b vl pop
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
  unconst @@ simpl @@ repls b s
;;
