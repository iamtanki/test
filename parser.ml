type token =
  | LPAREN
  | RPAREN
  | SEMISEMI
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | INTV of (int)

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
# 15 "parser.ml"
let yytransl_const = [|
  257 (* LPAREN *);
  258 (* RPAREN *);
  259 (* SEMISEMI *);
  260 (* PLUS *);
  261 (* MINUS *);
  262 (* TIMES *);
  263 (* DIV *);
    0|]

let yytransl_block = [|
  264 (* INTV *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\000\000"

let yylen = "\002\000\
\002\000\001\000\003\000\003\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\002\000\008\000\000\000\000\000\001\000\
\000\000\000\000\000\000\000\000\007\000\000\000\000\000\005\000\
\006\000"

let yydgoto = "\002\000\
\005\000\006\000"

let yysindex = "\003\000\
\002\255\000\000\002\255\000\000\000\000\014\255\009\255\000\000\
\002\255\002\255\002\255\002\255\000\000\251\254\251\254\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\020\255\024\255\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\253\255"

let yytablesize = 29
let yytable = "\007\000\
\011\000\012\000\003\000\001\000\000\000\014\000\015\000\016\000\
\017\000\004\000\013\000\000\000\009\000\010\000\011\000\012\000\
\008\000\009\000\010\000\011\000\012\000\003\000\003\000\003\000\
\003\000\004\000\004\000\004\000\004\000"

let yycheck = "\003\000\
\006\001\007\001\001\001\001\000\255\255\009\000\010\000\011\000\
\012\000\008\001\002\001\255\255\004\001\005\001\006\001\007\001\
\003\001\004\001\005\001\006\001\007\001\002\001\003\001\004\001\
\005\001\002\001\003\001\004\001\005\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  SEMISEMI\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  "

let yynames_block = "\
  INTV\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'Expr) in
    Obj.repr(
# 17 "parser.mly"
                 ( _1 )
# 91 "parser.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 20 "parser.mly"
        ( _1 )
# 98 "parser.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 21 "parser.mly"
                    ( _1 + _3)
# 106 "parser.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 22 "parser.mly"
                     ( _1 - _3)
# 114 "parser.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 23 "parser.mly"
                     ( _1 * _3)
# 122 "parser.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 24 "parser.mly"
                   ( _1 / _3)
# 130 "parser.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Expr) in
    Obj.repr(
# 25 "parser.mly"
                        ( _2 )
# 137 "parser.ml"
               : 'Expr))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : int)
