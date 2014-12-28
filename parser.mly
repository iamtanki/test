%{
%}

%token LPAREN RPAREN  SEMISEMI
%token PLUS MINUS TIMES DIV
%left PLUS MINUS
%left TIMES DIV
%nonassoc LPAREN RPAREN

%token <int> INTV
%start  main
%type <int> main

%%

main :
   Expr SEMISEMI { $1 }

Expr :
   INTV { $1 }
   | Expr PLUS Expr { $1 + $3}
   | Expr MINUS Expr { $1 - $3}
   | Expr TIMES Expr { $1 * $3}
   | Expr DIV Expr { $1 / $3}
   | LPAREN Expr RPAREN { $2 }
