{
  open  Parser
  exception Err
}

rule lexmain = parse
      [' ' '\t' ]+ {lexmain lexbuf}
  | "(" {Parser.LPAREN}
  | ")" {Parser.RPAREN}
  | ";;" {Parser.SEMISEMI}
  | "+" {Parser.PLUS}
  | "-"  {Parser.MINUS}
  | "*" {Parser.TIMES}
  | "/" {Parser.DIV}
  | ['0'-'9']+ {Parser.INTV (int_of_string (Lexing.lexeme lexbuf))}
  | eof {exit 0}
  | ['\n'] {raise Err}
