exception LexingError

type token = Id of string | Num of int 
  | Plus | Minus | Times | Divide | LPar | RPar
  | LThan | GThan | LTEq | GTEq | IsEqual | Different
  | Fun | Arrow | Equals | Let | In | If | Then | Else
  | True | False | And | Or | Not

let rec stringToCharlist s = 
  List.init (String.length s) (String.get s);;

let rec charlistToString s = 
  match s with
  | [] -> ""
  | hd::tl -> String.make 1 hd ^ (charlistToString tl)

let charToInt c =
  Char.code c - Char.code '0'

let rec charlistToNum s = 
  let rs = List.rev s in 
  let rec aux s = 
    match s with 
    | [] -> 0
    | hd::tl -> charToInt hd + 10*(charlistToNum tl)
  
  in aux rs

let isAlphanumerical c = 
  (Char.code c >= Char.code 'a' && Char.code c <= Char.code 'z') ||
  (Char.code c >= Char.code 'A' && Char.code c <= Char.code 'Z') || 
  (Char.code c >= Char.code '0' && Char.code c <= Char.code '9')

let isNumber c = 
  (Char.code c >= Char.code '0' && Char.code c <= Char.code '9')

let isNonZeroNumber c = 
  (Char.code c >= Char.code '1' && Char.code c <= Char.code '9')

let isLowercase c = 
  (Char.code c >= Char.code 'a' && Char.code c <= Char.code 'z')

let rec parseVar s = 
  match s with 
  | [] -> ([], [])
  | hd::tl -> if isAlphanumerical hd || Char.code hd = Char.code '_' then 
                let res = parseVar tl in
                (hd::(fst res), snd res)
              else 
                ([], s)

let parseId s = 
  if s <> [] then (
    if isLowercase (List.hd s) then let sp = parseVar (List.tl s) in 
      (List.hd s::(fst sp), snd sp)
    else
      ([], s))
  else
    ([], s)
      

let rec parseNums s =
  match s with 
  | [] -> ([], [])
  | hd::tl -> if isNumber hd then let res = parseNums tl in
                (hd::(fst res), snd res) else ([], s)

let rec parseNum s = 
  match s with
  | [] -> ([], [])
  | hd::tl -> if isNonZeroNumber hd then let res = parseNums tl in (hd::(fst res), snd res) else ([], [])

let rec lexer expr =
  match expr with
  | [] -> []
  | '\n'::q | '\t'::q | ' '::q -> lexer q
  | '-'::'>'::q -> Arrow::(lexer q)
  | '+'::q -> Plus::(lexer q)
  | '-'::q -> Minus::(lexer q)
  | '*'::q -> Times::(lexer q)
  | '/'::q -> Divide::(lexer q)
  | '('::q -> LPar::(lexer q)
  | ')'::q -> RPar::(lexer q)
  | '<'::'='::q -> LTEq::(lexer q)
  | '>'::'='::q -> GTEq::(lexer q)
  | '='::'='::q -> IsEqual::(lexer q)
  | '!'::'='::q -> Different::(lexer q)
  | '<'::q -> LThan::(lexer q)
  | '>'::q -> GThan::(lexer q)
  | '='::q -> Equals::(lexer q)
  | '&'::'&'::q -> And::(lexer q)
  | '|'::'|'::q -> Or::(lexer q)
  | '!'::q -> Not::(lexer q)
  | _ -> let (e, q) = parseNum expr in
          if (e <> []) then Num(charlistToNum e)::(lexer q) else (
            let (e2, q2) = parseId expr in
              match e2 with
              | [] -> raise LexingError
              | 'l'::'e'::'t'::[] -> Let::(lexer q2)
              | 'i'::'f'::[] -> If::(lexer q2)
              | 't'::'h'::'e'::'n'::[] -> Then::(lexer q2)
              | 'e'::'l'::'s'::'e'::[] -> Else::(lexer q2)
              | 'i'::'n'::[] -> In::(lexer q2)
              | 'f'::'u'::'n'::[] -> Fun::(lexer q2)
              | 't'::'r'::'u'::'e'::[] -> True::(lexer q2)
              | 'f'::'a'::'l'::'s'::'e'::[] -> False::(lexer q2)
              | _ -> Id(charlistToString e2)::(lexer q2)
          )
          