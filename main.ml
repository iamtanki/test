open Syntax
open Eval

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
    print_string "the value is :  "; print_v v;
    print_newline ();
    print_value ();
  with
    EvalError -> print_string "Operator Error"; print_newline ();print_value ()
  | DividedZero -> print_string "Divided Zero"; print_newline (); print_value ()
  |  _ -> print_string "Something wrong"; print_newline (); print_value ()

let _ = print_value ()
