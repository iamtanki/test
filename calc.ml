let _  =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      print_string "#";
      flush stdout ;
      let result = Parser.main Lexer.lexmain lexbuf in
      print_int result;
      print_newline ();
    done
  with
     Lexer.Err -> print_string "Error   dddd !\n"
    |  _ -> print_string "Error Input !!\n";;
