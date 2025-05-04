open Parser
open Lexer

exception TypingError

type return = Int of int | Bool of bool | App of return * return

val interpret : expression -> return