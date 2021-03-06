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
%token REC
%token MALLOC FREE DEREF REFASSIGN

%token<Syntax.id> ID
%token <int> INTV
// %token <bool> BOOLV

%start startpart
%type <Syntax.program> startpart

   %%

startpart :
   TOPExpr SEMISEMI {  Exp $1 }
    | DECL SEMISEMI{ Decl  $1 }
    | RECDECL SEMISEMI { $1 }

RECDECL :
     LET REC ID DEQ FUN ID FARROW TOPExpr { LetRecDecl ($3,$6,$8)  }

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
    | FunExpr { $1 }
    | RecExpr { $1 }

RecExpr :
     LET REC ID DEQ FUN ID FARROW TOPExpr IN TOPExpr { RecExp ($3,$6,$8,$10) }

FunExpr :
   FUN ID FARROW TOPExpr { FunExp ($2,$4) }

/*PARALIST :
*    ID { [$1] }
*   | PARALIST ID { $1 @ [$2] }
*/

LetExpr :
       LET ID DEQ TOPExpr IN TOPExpr {LetExp ($2, $4, $6)}
    | LET ID DEQ TOPExpr ANDDecl IN TOPExpr { LetAndExp ($2, $4, $5, $7)}
    | LET ID DEQ MALLOC IN TOPExpr {AllocExp ($2, $6)}

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
    | AppExpr { $1 }

AppExpr :
        AppExpr  RefAssignExpr { AppExp ($1,$2)}
    | RefAssignExpr { $1 }

RefAssignExpr :
    DerefExpr REFASSIGN RefAssignExpr { RefAssignExp ($1, $3)}
    | DerefExpr { $1 }

DerefExpr :
        DEREF DerefExpr { DerefExp ($2)}
    | VExpr { $1}

VExpr :
        INTV  {IntV $1}
    | TRUE {BoolV true}
    | FALSE {BoolV false}
    | ID { Var $1}
    | LPAREN TOPExpr RPAREN { $2 }
