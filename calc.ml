let _  =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Parser.main Lexer.lexmain lexbuf in
      print_int result;
      print_newline ();
      flush stdout
    done
  with
      _ -> print_string "Error Input !!\n";;
