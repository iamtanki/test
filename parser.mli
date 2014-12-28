type token =
  | LPAREN
  | RPAREN
  | SEMISEMI
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | INTV of (int)

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> int
