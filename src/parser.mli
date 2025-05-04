open Lexer
exception ParsingError

type expression = 
    Var of string | Num of int | True | False
  | Add of expression * expression
  | Mult of expression  * expression
  | Sub of expression * expression
  | Div of expression * expression
  | Let of expression * expression * expression
  | Fun of expression * expression
  | If of expression * expression * expression
  | Diff of expression * expression
  | IsEq of expression * expression
  | GrThan of expression * expression
  | LeThan of expression * expression
  | GrEq of expression * expression
  | LEq of expression * expression
  | And of expression * expression
  | Or of expression * expression
  | Not of expression

val parser : token list -> expression