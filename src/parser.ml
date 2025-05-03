open Lexer 
exception ParsingError
exception TypingError

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

let parser tokens =
  let vartypes = Hashtbl.create 100000 in

  let rec parseExpr tks =
    let (e, q, t) = parseEquality tks in
    (e, q, t)

  and parseEquality tks =
    let (e, q, t) = parseComp tks in
    match q with 
    | IsEqual::tl -> let (e2, q2, t2) = parseEquality tl in 
        if t = t2 then (IsEq(e, e2), q2, Bool)
        else raise TypingError
    | Different::tl -> let (e2, q2, t2) = parseEquality tl in 
        if t = t2 then (Diff(e, e2), q2, Bool)
        else raise TypingError
    | _ -> (e, q, t)

  and parseComp tks =
    let (e, q, t) = parseTerm tks in
    match q with 
    | LThan::tl -> let (e2, q2, t2) = parseComp tl in 
        if t = Int && t2 = Int then (LeThan(e, e2), q2, Bool) 
        else raise TypingError
    | GThan::tl -> let (e2, q2, t2) = parseComp tl in 
        if t = Int && t2 = Int then (GrThan(e, e2), q2, Bool)
        else raise TypingError
    | LTEq::tl -> let (e2, q2, t2) = parseComp tl in 
        if t = Int && t2 = Int then (LEq(e, e2), q2, Bool)
        else raise TypingError
    | GTEq::tl -> let (e2, q2, t2) = parseComp tl in 
        if t = Int && t2 = Int then (GrEq(e, e2), q2, Bool)
        else raise TypingError
    | And::tl -> let (e2, q2, t2) = parseComp tl in 
        if t = Bool && t2 = Bool then (And(e, e2), q2, Bool)
        else raise TypingError
    | Or::tl -> let (e2, q2, t2) = parseComp tl in 
        if t = Bool && t2 == Bool then (Or(e, e2), q2, Bool)
        else raise TypingError
    | _ -> (e, q, t)

  and parseTerm tks =
    let (e, q, t) = parseFactor tks in
    match q with 
    | Minus::tl -> let (e2, q2, t2) = parseTerm tl in 
        if t = Int && t2 = Int then (Sub(e, e2), q2, Int)
        else raise TypingError
    | Plus::tl -> let (e2, q2, t2) = parseTerm tl in 
        if t = Int && t2 = Int then (Add(e, e2), q2, Int)
        else raise TypingError
    | _ -> (e, q, t)
  
  and parseFactor tks = 
    let (e, q, t) = parseUnary tks in
    match q with
    | Divide::tl -> let (e2, q2, t2) = parseFactor tl in 
        if t = Int && t2 = Int then (Div(e, e2), q2, Int)
        else raise TypingError
    | Times::tl -> let (e2, q2, t2) = parseFactor tl in 
        if t = Int && t2 = Int then (Mult(e, e2), q2, Int)
        else raise ParsingError
    | _ -> (e, q, t)

  and parseUnary tks = 
    match tks with
    | Not::tl -> let (e, q, t) = parseUnary tl in 
        if t = Bool then (Not(e), q, Bool) 
        else raise TypingError
    | Minus::tl -> let (e, q, t) = parseUnary tl in 
        if t = Int then (Sub(Num(0), e), q, Int)
        else raise TypingError 
    | _ -> parsePrimary tks

  and parsePrimary tks = 
    match tks with
    | [] -> raise ParsingError
    | True::tl -> (True, tl, Bool)
    | False::tl -> (False, tl, Bool)
    | Id(s)::tl -> if Hashtbl.mem vartypes s then (Var(s), tl, Hashtbl.find vartypes s) 
                   else raise ParsingError
    | Num(n)::tl -> (Num(n), tl, Int)
    | LPar::tl -> let (e, q, t) = parseExpr tl in (
                  match q with
                  | RPar::tl2 -> (e, tl2, t)
                  | _ -> raise ParsingError )
    | Let::tl -> let (e, q, t) = parseExpr tl in 
                  ( match q with 
                  | Equals::tl2 -> let (e2, q2, t2) = parseExpr tl2 in
                                  ( match (q2, e) with 
                                    | (In::tl3, Var(s)) -> let (e3, q3, t3) = parseExpr tl3 in
                                          Hashtbl.add vartypes s t2;
                                          (Let(e, e2, e3), q3, t3)
                                    | _ -> raise ParsingError
                                  )
                  | _ -> raise ParsingError )
    | If::tl -> let (e, q, t) = parseEquality tl in
                  ( match q with 
                    | Then::tl2 -> let (e2, q2, t2) = parseExpr tl2 in
                                  (match q2 with
                                   | Else::tl3 -> let (e3, q3, t3) = parseExpr tl3 in 
                                      if t = Bool && t2 = t3 then (If(e, e2, e3), tl3, t2) 
                                      else raise TypingError
                                   | _ -> raise ParsingError
                                  )
                    | _ -> raise ParsingError
                  )
    | _ -> raise ParsingError
    
  in
  let res = parseExpr tokens in
  match res with 
  | (finExpr, residue, finType) -> if residue = [] 
      then {value = finExpr; typ = finType}
      else raise ParsingError