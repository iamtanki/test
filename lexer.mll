{
  let reservedWords =
    [
      ("if", Parser.IF);
      ("then", Parser.THEN);
      ("else", Parser.ELSE);
      ("true", Parser.TRUE);
      ("false", Parser.FALSE);
      ("let", Parser.LET);
      ("in", Parser.IN)
    ]
}
rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }

| "-"? ['0'-'9']+
    { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| ";;" { Parser.SEMISEMI }
| "+" { Parser.PLUS }
| "-" { Parser.MINUS}
| "*" { Parser.TIMES }
| "/" { Parser.DIV}
| "||" { Parser.OR}
| "&&" { Parser.AND}
| "!" { Parser.NOT}
| ">" { Parser.GT}
| "<" { Parser.LT }
| "==" {Parser.EQ} (* logic eq, not assignment *)
| "=" { Parser.DEQ} (* assignment *)
| "(*" { comments 0 lexbuf  }

| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try
        List.assoc id reservedWords
      with
      _ -> Parser.ID id
     }
| eof { exit 0 }
and comments level = parse
    | "*)" { if level = 0 then main lexbuf else comments (level - 1) lexbuf }
    | "(*" { comments (level + 1) lexbuf }
    | _ { comments level lexbuf }
