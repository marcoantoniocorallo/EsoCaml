type 'v env = (string * 'v) list

let rec lookup env x =
  match env with
  | []        -> raise(Exceptions.Binding_Error(x ^ " not found"))
  | (y, v)::r -> if x=y then v else lookup r x
;;

(* alias: identifiers are strings *)
type ide = string

(** Located node *)
type 'a located = { loc : Lexing.position * Lexing.position; value : 'a}

(** Recursive definition of polymorphic data structures *)
type 'a sequence = 
  | Nil                                             (* Represents the empty sequence *)
  | Cons of 'a * 'a sequence                        (* Item of the sequence *)
[@@deriving show]
;;

(** Algebraic Data Types *)
type exp =
	| CstI of int                              	 			(* Integer constants *)
	| CstB of bool                               			(* Boolean constants *)
	| CstF of float															 			(* Float constants *)
	| CstC of char															 			(* Char literals *)
	| CstS of string														 			(* String literals *)
	| Not of located_exp															(* Negation of a bool exp *)
  | Neg of located_exp															(* Negation of an int exp *)
	| Var of ide                              	 			(* Variables/Identifiers *)
	| Let of ide * ttype option * located_exp * located_exp        	
                                                    (* Typed declaration *)
	| Prim of located_exp * ide * located_exp         (* Op Primitives *)
	| If of located_exp * located_exp * located_exp   (* If-then-else *)
	| Letfun of ide * ide * ttype * located_exp * located_exp 
                                                    (* Fun declaration 
                                                    (f, x, type of f, fBody, letBody)  *)
  | Lambda of ide * ttype * located_exp             (* lambda (x, type of f, body) *)
	| Call of located_exp * located_exp               (* Fun application *)
	| Tup of located_exp sequence			 				 	 			(* Heterogeneous Fixed-length list of expressions *)
	| Proj of located_exp * located_exp               (* i-th element of tuple *)
	| Lst of located_exp sequence 		 					 			(* Homogeneous List of expressions *)
	| Cons_op of located_exp * located_exp						(* Concatenates an exp in head of a list *)
	| Head of located_exp															(* Return the first element of a list *)
	| Tail of located_exp															(* Return the list without the first el *)

(** Types definition *)
and ttype = 
  | Tint                                            (*  Type int *)
  | Tbool                                           (*  Type bool *)
  | Tfloat                                          (*  Type float *)
  | Tchar                                           (*  Type char *)
  | Tstring                                         (*  Type string *)
  | Tfun of ttype * ttype                           (*  Type of function *)
  | Ttuple of ttype sequence                        (*  Compound type: tuple *)
  | Tlist of ttype option                           (*  Compound type: list *)

and located_exp = exp located                 			(* ( exp * location ) *)

(** Expressible and denotable values. 
 *  A runtime value is an integer, a boolean, a float, a char, a string,
 *	a tuple or a list of values or a function closure.
 *)
and value =
	| Int of int
	| Bool of bool
	| Float of float
	| Char of char
	| String of string
	| Closure of string * string * located_exp * value env	(* (f, x, fBody, fDeclEnv) *)
	| Tuple of value sequence   														(* Heterogeneous fixed-length tuple of values*)
	| ListV of value sequence   														(* Homogeneous list of values *)
;;