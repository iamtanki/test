%{
  open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MINUS TIMES DIV
%token TRUE FALSE
%token IF THEN ELSE
%token AND OR
%token LT EQ GT
%token NOT

%token LET IN DEQ

%token<Syntax.id> ID
%token <int> INTV
%token <bool> BOOLV

%start startpart
%type <Syntax.program> startpart

   %%

startpart :
   TOPExpr SEMISEMI {  Exp $1 }
   | LET ID DEQ TOPExpr SEMISEMI { Decl ($2, $4) }

TOPExpr :
     IFExpr { $1 }
   | LetExpr { $1 }

LOExpr :
    LOExpr OR LAExpr {BinOp (Or , $1, $3)}
    | LAExpr { $1 }

LAExpr :
    LAExpr AND LTExpr {BinOp (And, $1, $3)}
    | LTExpr { $1 }

LTExpr:
    LTExpr LT Expr { BinOp (Lt , $1, $3)}
    | LTExpr GT Expr { BinOp (Gt, $1, $3)}
    | Expr { $1 }

Expr :
    Expr PLUS MExpr { BinOp ( Plus, $1, $3)}
    | Expr MINUS MExpr {BinOp (Minus , $1, $3)}
    | MExpr { $1 }

MExpr :
        MExpr TIMES SExpr {BinOp (Times, $1, $3)}
    | MExpr DIV SExpr { BinOp (Div, $1, $3) }
    | SExpr { $1 }

SExpr :
        NOT  SExpr { SingleOp (Not, $2)}
    | VExpr { $1 }

VExpr :
        INTV  {IntV $1}
    | TRUE {BoolV true}
    | FALSE {BoolV false}
    | ID { Var $1}
    | LPAREN TOPExpr RPAREN { $2 }

LetExpr :
   LET ID DEQ TOPExpr IN TOPExpr {LetExp ($2, $4, $6)}

IFExpr :
    IF IFExpr THEN IFExpr ELSE IFExpr {IfExp ($2, $4,$6)}
    | LOExpr { $1 }
