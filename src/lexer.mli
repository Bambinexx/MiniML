exception LexingError

type expression = Id of string | Num of int 
  | Plus | Minus | Times | Divides | LPar | RPar
  | LThan | GThan | LTEq | GTEq | IsEqual | Different
  | Fun | Arrow | Equals | Let | In | If | Then | Else

val charlistToString : char list -> string
val charToInt : char -> int
val charlistToNum : char list -> int
val isAlphanumerical : char -> bool
val isNumber : char -> bool
val isNonZeroNumber : char -> bool
val isLowercase : char -> bool
val parseVar : char list -> char list * char list
val parseId : char list -> char list * char list
val parseNums : char list -> char list * char list
val parseNum : char list -> char list * char list
val lexer : char list -> expression list