open Lexer
open Parser
open Interpret

let () =
  let filename = Sys.argv.(1) in 
  let file = In_channel.with_open_text filename In_channel.input_all in

  match interpret (parser (lexer (stringToCharlist file))) with
  | Int(n) -> Printf.printf "int : %i\n" n
  | Bool(b) -> if b then Printf.printf "bool : true\n" else Printf.printf "bool : false\n"
  | _ -> failwith "not implemented yet"