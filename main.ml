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

let read_file s =
  let ic = open_in "program.txt" in
  try
    while true do
      let line = input_line ic in
      s := (!s) ^ line;
    done
  with
    _ -> close_in ic

let rec print_value () =
  try
    let s = ref  "" in
    read_file s;
    let lb = Lexing.from_string !s in
    print_string "# " ;
    flush stdout;
    let exp = Parser.startpart Lexer.main lb in
    let v = eval_program exp in
    let ty = ty_program exp in
    print_string  " val : ";  print_ty ty; print_string " = ";  print_v v;
    print_newline ();
    (* print_value (); *)
  with
    Err e -> print_string e; print_newline ()
    | _ -> print_string "Not Complete"; print_newline ()

let _ = print_value ()
