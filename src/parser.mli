open Lexer
exception ParsingError

type ltype = Int | Bool | App of ltype * ltype

type expr = 
    Var of string | Num of int | True | False
  | Add of expr * expr
  | Mult of expr  * expr
  | Sub of expr * expr
  | Div of expr * expr
  | Let of expr * expr * expr
  | Fun of expr * expr
  | If of expr * expr * expr
  | Diff of expr * expr
  | IsEq of expr * expr
  | GrThan of expr * expr
  | LeThan of expr * expr
  | GrEq of expr * expr
  | LEq of expr * expr
  | And of expr * expr
  | Or of expr * expr
  | Not of expr

type expression = {value : expr; typ : ltype}

val parser : token list -> expression