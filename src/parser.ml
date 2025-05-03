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

let parser tokens = 
  let rec parseExpr tks =
    let (e, q) = parseEquality tks in
    (e,q)

  and parseEquality tks =
    let (e, q) = parseComp tks in
    match q with 
    | IsEqual::tl -> let (e2, q2) = parseEquality tl in (IsEq(e, e2), q2)
    | Different::tl -> let (e2, q2) = parseEquality tl in (Diff(e, e2), q2)
    | _ -> (e, q)

  and parseComp tks =
    let (e, q) = parseTerm tks in
    match q with 
    | LThan::tl -> let (e2, q2) = parseComp tl in (LeThan(e, e2), q2)
    | GThan::tl -> let (e2, q2) = parseComp tl in (GrThan(e, e2), q2)
    | LTEq::tl -> let (e2, q2) = parseComp tl in (LEq(e, e2), q2)
    | GTEq::tl -> let (e2, q2) = parseComp tl in (GrEq(e, e2), q2)
    | And::tl -> let (e2, q2) = parseComp tl in (And(e, e2), q2)
    | Or::tl -> let (e2, q2) = parseComp tl in (Or(e, e2), q2)
    | _ -> (e, q)

  and parseTerm tks =
    let (e, q) = parseFactor tks in
    match q with 
    | Minus::tl -> let (e2, q2) = parseTerm tl in (Sub(e, e2), q2)
    | Plus::tl -> let (e2, q2) = parseTerm tl in (Add(e, e2), q2)
    | _ -> (e, q)
  
  and parseFactor tks = 
    let (e, q) = parseUnary tks in
    match q with
    | Divide::tl -> let (e2, q2) = parseFactor tl in (Div(e, e2), q2)
    | Times::tl -> let (e2, q2) = parseFactor tl in (Mult(e, e2), q2)
    | _ -> (e, q)

  and parseUnary tks = 
    match tks with
    | Not::tl -> let (e, q) = parseUnary tl in (Not(e), q)
    | Minus::tl -> let (e, q) = parseUnary tl in (Sub(Num(0), e), q)
    | _ -> parsePrimary tks

  and parsePrimary tks = 
    match tks with
    | [] -> raise ParsingError
    | True::tl -> (True, tl)
    | False::tl -> (False, tl)
    | Id(s)::tl -> (Var(s), tl)
    | Num(n)::tl -> (Num(n), tl)
    | LPar::tl -> let (e, q) = parseExpr tl in (
                  match q with
                  | RPar::tl2 -> (e, tl2)
                  | _ -> raise ParsingError )
    | Let::tl -> let (e, q) = parseExpr tl in 
                  ( match q with 
                  | Equals::tl2 -> let (e2, q2) = parseExpr tl2 in
                                   ( match q2 with 
                                    | In::tl3 -> let (e3, q3) = parseExpr tl3 in (Let(e, e2, e3), q3) 
                                    | _ -> raise ParsingError
                                    )
                  | _ -> raise ParsingError )
    | If::tl -> let (e, q) = parseEquality tl in
                  ( match q with 
                    | Then::tl2 -> let (e2, q2) = parseExpr tl2 in
                                  (match q2 with
                                   | Else::tl3 -> let (e3, q3) = parseExpr tl3 in (If(e, e2, e3), tl3)
                                   | _ -> raise ParsingError
                                  )
                    | _ -> raise ParsingError
                  )
    | _ -> raise ParsingError
    
  in
  fst (parseExpr tokens)