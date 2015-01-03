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
%token LET IN DEQ EAND
%token FUN FARROW

%token<Syntax.id> ID
%token <int> INTV
%token <bool> BOOLV

%start startpart
%type <Syntax.program> startpart

   %%

startpart :
   TOPExpr SEMISEMI {  Exp $1 }
    | DECL SEMISEMI{ Decl  $1 }

DECL :
    LET ID DEQ TOPExpr {SingleDecl ($2, $4)}
    | LET ID DEQ TOPExpr DECL {CompDecl ($2, $4, $5)}
    | LET ID DEQ TOPExpr  ANDDecl { AndDecl ($2, $4, $5)}

ANDDecl :
        EAND ID DEQ TOPExpr { SingleAndDecl ($2,$4)}
    | EAND ID DEQ  TOPExpr ANDDecl {CompAndDecl ($2, $4, $5)}


TOPExpr :
     IFExpr { $1 }
    | LetExpr { $1 }


LetExpr :
       LET ID DEQ TOPExpr IN TOPExpr {LetExp ($2, $4, $6)}
   | LET ID DEQ TOPExpr ANDDecl IN TOPExpr { LetAndExp ($2, $4, $5, $7)}

IFExpr :
    IF IFExpr THEN IFExpr ELSE IFExpr {IfExp ($2, $4,$6)}
    | LOExpr { $1 }

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
