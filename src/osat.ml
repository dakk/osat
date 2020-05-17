open Printf;;

let main () = 
  if Array.length Sys.argv < 2 then printf "usage: %s file.cnf\n" Sys.argv.(0) else (
    let f = Sys.argv.(1) in
    printf "loading %s... %!" f;
    let t = Parser.parse_cnf3_file f in 
    printf "done\nrunning solver...\n%!";
    try 
      let s = Solver.DPLL.solve t in
      printf "solution found:\n%!";
      Solver.print s
      (* printf "\nverify sol: %b\n" (Solver.verify t s) *)
    with | _ -> printf "no solution found\n%!";
  )
in main();;