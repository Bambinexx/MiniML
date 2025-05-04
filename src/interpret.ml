open Parser
open Lexer

exception TypingError

type return = Int of int | Bool of bool | App of return * return

let interpret (expr : expression) : return =
  let variables = Hashtbl.create 100000 in

  let rec interpretc expr = 
    match expr with
    | Var(s) -> Hashtbl.find variables s
    | True -> Bool(true)
    | False -> Bool(false)
    | Num(n) -> Int(n)
    | Add(e1, e2) -> let (res1, res2) = (interpretc e1, interpretc e2) in (
        match (res1, res2) with
        | (Int(n1), Int(n2)) -> Int(n1 + n2)
        | _ -> raise TypingError)
    | Sub(e1, e2) -> let (res1, res2) = (interpretc e1, interpretc e2) in (
        match (res1, res2) with
        | (Int(n1), Int(n2)) -> Int(n1 - n2)
        | _ -> raise TypingError )
    | Mult(e1, e2) -> let (res1, res2) = (interpretc e1, interpretc e2) in (
        match (res1, res2) with
        | (Int(n1), Int(n2)) -> Int(n1*n2)
        | _ -> raise TypingError )
    | Div(e1, e2) -> let (res1, res2) = (interpretc e1, interpretc e2) in (
        match (res1, res2) with
        | (Int(n1), Int(n2)) -> Int(n1 / n2)
        | _ -> raise TypingError )
    | IsEq(e1, e2) -> let (res1, res2) = (interpretc e1, interpretc e2) in (
        match (res1, res2) with
        | (Int(n1), Int(n2)) -> Bool(n1 = n2)
        | (Bool(b1), Bool(b2)) -> Bool(b1 = b2)
        | _ -> raise TypingError )
    | Diff(e1, e2) -> let (res1, res2) = (interpretc e1, interpretc e2) in (
        match (res1, res2) with
        | (Int(n1), Int(n2)) -> Bool(n1 <> n2)
        | (Bool(b1), Bool(b2)) -> Bool(b1 <> b2)
        | _ -> raise TypingError )
    | GrThan(e1, e2) -> let (res1, res2) = (interpretc e1, interpretc e2) in (
        match (res1, res2) with
        | (Int(n1), Int(n2)) -> Bool(n1 > n2)
        | _ -> raise TypingError )
    | LeThan(e1, e2) -> let (res1, res2) = (interpretc e1, interpretc e2) in (
        match (res1, res2) with
        | (Int(n1), Int(n2)) -> Bool(n1 < n2)
        | _ -> raise TypingError )
    | GrEq(e1, e2) -> let (res1, res2) = (interpretc e1, interpretc e2) in (
        match (res1, res2) with
        | (Int(n1), Int(n2)) -> Bool(n1 >= n2)
        | _ -> raise TypingError )
    | LEq(e1, e2) -> let (res1, res2) = (interpretc e1, interpretc e2) in (
        match (res1, res2) with
        | (Int(n1), Int(n2)) -> Bool(n1 <= n2)
        | _ -> raise TypingError )
    | Or(e1, e2) -> let (res1, res2) = (interpretc e1, interpretc e2) in (
        match (res1, res2) with
        | (Bool(b1), Bool(b2)) -> if b1 || b2 then Bool(true) else Bool(false)
        | _ -> raise TypingError )
    | And(e1, e2) -> let (res1, res2) = (interpretc e1, interpretc e2) in (
        match (res1, res2) with
        | (Bool(b1), Bool(b2)) -> if b1 && b2 then Bool(true) else Bool(false)
        | _ -> raise TypingError )
    | Not(e) -> let res = interpretc e in (
        match res with
        | Bool(true) -> Bool(false)
        | Bool(false) -> Bool(true) 
        | _ -> raise TypingError)
    | If(cond, t, f) -> let (resc, rest, resf) = (interpretc cond, interpretc t, interpretc f) in (
        match resc with
        | Bool(true) -> rest
        | Bool(false) -> resf
        | _ -> raise TypingError )
    | Let(var, value, f) -> let res = interpretc value in (
        match (var, res) with 
        | (Var(s), Bool(b)) -> Hashtbl.add variables s (Bool(b)); interpretc f
        | (Var(s), Int(n)) -> Hashtbl.add variables s (Int(n)); interpretc f
        | _ -> raise TypingError )
    | _ -> failwith "Not implemented yet"
  
  in interpretc expr