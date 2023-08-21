(*
 * This is a simple EsoLang, quickly developed in my free time.
 * To be honest, this is an esoteric front-end of a simple PL I developed early.
 * Thus, this is a functional EsoLang inspired by reMorse.
 *
 * Each constant and identifier must be written in morse code.
 * Each constant must begin with 3 letters that specify the type:
 * INTxxx, CHRxxx, FLTxxx, BOLxxx, STRxxx.
 * If there is no type at the beginning of the word, It will be treated as an identifier.
 *
 * Keywords, Operators and types (for type annotations) are divided in three rotating wheels
 * You can handle the rotating wheels simply with
 *  '>' for increasing by 1 the keywords wheel;
 *  '<' for increasing by 1 the operators wheel;
 *  '^' for increasing by 1 the types wheel;
 *  '!' for choosing the current keyword;
 *  '@' for choosing the current operator;
 *  '#' for choosing the current type;
 * When you choose a keyword, an operator or a type, the corresponding wheel is restarted.
 * In addition to these keywords and operators, you can use the following special ones
 * ["," "(" ")" "[" "]" ":" "->"] as you should use them in OCaml.
 *
 * For simplicity, you can use the character '|' as separator between morse code statements
 * and the character '_' as space between two morse code words.
 *
 * Since the only legal characters are: . - > < ^ ! @ # , ( ) [ ] : _
 * every other character will be ignored and can be used for comments.
 *
 * Due to the complexity of this language, it has been tested very little.
 * This is just a toy language, but if you want to play with it, please, 
 * let me know what do you thing about it! :)
 *)

{
	open Exceptions
  open Parser
  open Utils

	let create_hashtable size init =
		let tbl = Hashtbl.create size in
		List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
		tbl

  (* Rotating wheel for the keywords *)
  let kws = [
    LET; IN; IF; THEN; ELSE; FUN; LAMBDA; NOT; AND; OR; PROJ;
    HEAD; TAIL; 
  ]

  (* Rotating wheel for the operators *)
  let ops = [
    PLUS; MINUS; TIMES; DIV; FPLUS; FMINUS; FTIMES; FDIV; EQ;
    NEQ; LESS; GREATER; LEQ; GEQ; CONCAT; CONS_OP; PIPE;
  ]

  (* Rotating wheel for the types *)
  let types = [ TINT; TCHAR; TFLOAT; TBOOL; TSTRING; TLIST ]

  (* morse alphabet *)
  let morse_table = 
    create_hashtable 27 [
      (".-",   "A");
      ("-...", "B");
      ("-.-.", "C");
      ("-..",  "D");
      (".",    "E");
      ("..-.", "F");
      ("--.",  "G");
      ("....", "H");
      ("..",   "I");
      (".---", "J");
      ("-.-",  "K");
      (".-..", "L");
      ("--",   "M");
      ("-.",   "N");
      ("---",  "O");
      (".--.", "P");
      ("--.-", "Q");
      (".-.",  "R");
      ("...",  "S");
      ("-",    "T");
      ("..-",  "U");
      ("...-", "V");
      (".--",  "W");
      ("-..-", "X");
      ("-.--", "Y");
      ("--..", "Z");

      (".----","1");
      ("..---","2");
      ("...--","3");
      ("....-","4");
      (".....","5");
      ("-....","6");
      ("--...","7");
      ("---..","8");
      ("----.","9");
      ("-----","0");

      (".-.-.-", ".");
      ("--..--", ",");
      ("..--..", "?");
      (".----.", "'");
      ("-.-.--", "!");
      ("-..-.", "/");
      ("-.--.", "(");
      ("-.--.-", ")");
      (".-...", "&");
      ("-.-.-.", ";");
      ("---...", ":");
      ("-...-", "=");
      (".-.-.", "+");
      ("-....-", "-");
      ("..--.-", "_");
      (".-..-.", "\"");
      ("...-..-", "$");
      (".--.-.", "@");
      
    ]

	let symbols =
		create_hashtable 22 [
			  (",", COMMA);
        ("(", LPAREN);
        (")", RPAREN);
        ("[", LBRACKET);
        ("]", RBRACKET);
        (":", COLON);
        ("->", ARROW)
		]
}

(** Definition section *)
let morse   = ['.' '-']+
let sep     = '|'
let space   = '_'
let white   = [' ' '\t']
let syms    = [',' '(' ')' '[' ']' ':' ] | "->"

rule tokenize s index = parse
	| morse as code     {
                        try tokenize (s^(Hashtbl.find morse_table code)) index lexbuf
                        with Not_found -> raise (Lexing_Error("Unexpected character: "^(Lexing.lexeme lexbuf)^" at "^
												  (string_of_position (Lexing.lexeme_start_p lexbuf))))
                      }
  | space             { tokenize (s^" ") index lexbuf }                      
  | syms as sym       { 
                        try Hashtbl.find symbols sym
                        with Not_found -> raise ((Lexing_Error("Unexpected character: "^(Lexing.lexeme lexbuf)^" at "^
												  (string_of_position (Lexing.lexeme_start_p lexbuf)))))
                      }
  | sep               {
                        try
                          let value = String.sub s 3 ((String.length s)-3) 
                              |> String.lowercase_ascii in
                          match (String.sub s 0 3) with
                          | "INT" -> INT(int_of_string value)
                          | "FLT" -> FLOAT(float_of_string value)
                          | "BOL" -> BOOL(bool_of_string value)
                          | "CHR" -> CHAR (value.[1])
                          | "STR" -> STRING value
                          | _ -> ID s
                        with Invalid_argument(_) -> ID s
                      }
  | '>' | '<' | '^'   { tokenize s (index+1) lexbuf }
  | '!'               { List.nth kws (index mod (List.length kws)) }
  | '@'               { List.nth ops (index mod (List.length ops)) }
  | '#'               { List.nth types (index mod (List.length types)) }
  | '\n'              { Lexing.new_line lexbuf; tokenize s index lexbuf }
  | eof               { EOF }
  | white             (* eat up whitespace *)
	| _ 			          (* Any other character will be ignored *)
                      { tokenize s index lexbuf }
