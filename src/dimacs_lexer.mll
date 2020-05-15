{
  open Parser
  open Lexing
}

let digit = ['0'-'9']
let var = digit*
let nvar = '-' digit*

rule token = parse
  | var                         { VAR (int_of_string (Lexing.lexeme lexbuf)) }
  | nvar                        { NVAR (abs @@ int_of_string (Lexing.lexeme lexbuf)) }
  | 'c'                         { C }
  | 'p'                         { P }
  | "\r\n"? | '\n'?             { new_line lexbuf ; EOL }
  | _ as c                      { failwith (Format.sprintf "invalid string starting with %C" c) }
