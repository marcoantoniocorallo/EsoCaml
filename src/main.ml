open Exceptions;;
open Utils;;
open Type_system;;

let eval code env = 
  let _ = type_check code in 
  print_endline (string_of_value (Interpreter.eval code env)) 

let print_usage () = 
  print_endline "You must specify the input file."

let () =
  let argvLength = Array.length Sys.argv in 
  if argvLength < 2 then print_usage()
  else
    let filename = Sys.argv.(argvLength-1) in 
    let lexbuf = Lexing.from_channel (open_in filename) in 
      try
        let code = Parser.main (Lexer.tokenize "" 0) lexbuf in eval code []
      with
        (* Character that doesn't matches any case in lexer (i.e. '&')*)
        |Lexing_Error(s) -> Printf.fprintf stderr "%s\n" s
        (* A malformed sequence of tokens (i.e. "let x + 5" ) *)
        |Parser.Error ->  Printf.fprintf stderr "Syntax error at %s.\n%!" 
                          (string_of_position (Lexing.lexeme_start_p lexbuf))
        (* Exceptions raised by the interpreter (i.e. a type error ) *)
        |exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn)