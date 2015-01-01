exception Err of string;;

type id = string

type binOp = Plus | Minus | Times | Div | And | Or | Lt | Eq | Gt
type singleOp = Not


type exp =
    Var of id
  | IntV of int
  | BoolV of bool
  | BinOp of binOp * exp * exp
  | SingleOp of singleOp * exp
  | IfExp of exp * exp * exp
  | LetExp of id * exp * exp
  | LetAndExp of id * exp * anddecl * exp
 and anddecl =
   SingleAndDecl of id * exp
  | CompAndDecl of id *exp * anddecl

type decl =
    SingleDecl of id * exp
  | CompDecl of id * exp * decl
  | AndDecl of id * exp * anddecl

type program =
    Exp of exp
  | Decl of decl

(* types *)
type tyexp = TyInt | TyBool
