open Syntax
open Eval
open Typing

let print_v exp =
  match exp with
    IntV  i -> print_string ( string_of_int i)
  | BoolV i -> print_string (string_of_bool i)

let print_ty ty = match ty with
  | TyInt -> print_string "int"
  | TyBool -> print_string "bool"

let rec print_value () =
  try
    let lb = Lexing.from_channel stdin in
    print_string "#" ;
    flush stdout;
    let exp = Parser.startpart Lexer.main lb in
    let v = eval_program exp in
    let ty = ty_program exp in
    print_string  "Val : ";  print_ty ty; print_string " = ";  print_v v;
    print_newline ();
    print_value ();
  with
    Err e -> print_string e; print_newline (); print_value ()
  | _ -> print_string "Not Complete"; print_newline (); print_value ()

let _ = print_value ()
