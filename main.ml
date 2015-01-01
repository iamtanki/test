open Syntax
open Eval
open Typing
open Environment

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

let rec print_value env tyenv =
  try
    (* let s = ref  "" in *)
    (* read_file s; *)
    let lb = Lexing.from_channel stdin in
    print_string "# " ;
    flush stdout;
    let exp = Parser.startpart Lexer.main lb in
    let (s, v , newenv) = eval_program exp env in
    let (ts, tv, newtyenv) = ty_program exp tyenv in
    print_string  (s ^ " : " ) ;
    print_ty tv;
    print_string " = ";
    print_v v;
    print_newline ();
    print_value newenv newtyenv;
  with
    Err e -> print_string e; print_newline ();     print_value env tyenv
    | _ -> print_string "Parsing Error: Not Complete";  print_newline ();  print_value env tyenv
let _ = print_value Environment.empty Environment.empty
