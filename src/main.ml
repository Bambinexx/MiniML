open Lexer
open Parser

let () = 
  let filename = Sys.argv.(1) in 
  let file = In_channel.with_open_text filename In_channel.input_all in

  let expr = parser (lexer (stringToCharlist file)) in
  Printf.printf "okay this works"