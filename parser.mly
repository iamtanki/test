%{
%}

%token LPAREN RPAREN  SEMISEMI
%token PLUS MINUS
%token TIMES DIV

%token <int> INTV
%start  main
%type <int> main

%%

main :
   Expr SEMISEMI { $1 }

Expr :
      | Expr PLUS MExpr { $1 + $3}
      | Expr MINUS MExpr { $1 - $3}
      | MExpr { $1 }

MExpr :
   | MExpr TIMES PExpr { $1 * $3}
   | MExpr DIV PExpr { $1 / $3}
   | PExpr { $1 }

PExpr:
   | INTV { $1 }
   | LPAREN Expr RPAREN { $2 }
