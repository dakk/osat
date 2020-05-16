open Printf;;

let main () = 
  if Array.length Sys.argv < 2 then printf "usage: %s file.cnf\n" Sys.argv.(0) else (
    let f = Sys.argv.(1) in
    printf "loading %s... %!" f;
    let t = Parser.parse_file f in 

    printf "done\noptimizing... %!";
    let s', t' = Bexp.same_polarity_removal t in
    printf "done (%d removed)\nrunning solver...\n%!" (List.length s');
    try 
      let s = Solver.solve t' @ s' in
      printf "solution found:\n%!";
      Solver.print s;
      printf "\nverify sol: %b\n" (Solver.verify t s)
    with | _ -> printf "no solution found\n%!";
  )
in main();;