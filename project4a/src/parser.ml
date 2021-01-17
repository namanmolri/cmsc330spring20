open SmallCTypes
open Utils
open TokenTypes

(* Parsing helpers (you don't need to modify these) *)

(* Return types for parse_stmt and parse_expr *)
type stmt_result = token list * stmt
type expr_result = token list * expr

(* Return the next token in the token list, throwing an error if the list is empty *)
let lookahead (toks : token list) : token =
  match toks with
  | [] -> raise (InvalidInputException "No more tokens")
  | h::_ -> h

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))


let rec parse_expr toks : expr_result =
  if toks <> [EOF] then
    parse_expr_or toks
  else
    raise (InvalidInputException "parse_expr1")

and parse_expr_or toks = 
  let (t, e) = parse_expr_and toks in
  match lookahead t with
  | Tok_Or -> let ta = match_token t Tok_Or in
              let (tb, e') = parse_expr_or ta in
              (tb, Or (e, e'))
  | _ -> t, e

and parse_expr_and toks = 
  let (t, e) = parse_expr_equality toks in
  match lookahead t with
  | Tok_And -> let ta = match_token t Tok_And in
              let (tb, e') = parse_expr_and ta in
              (tb, And (e, e'))
  | _ -> t, e

and parse_expr_equality toks = 
  let (t, e) = parse_expr_relational toks in
  match lookahead t with
  | Tok_Equal -> let ta = match_token t Tok_Equal in
              let (tb, e') = parse_expr_equality ta in
              (tb, Equal (e, e'))
  | Tok_NotEqual -> let ta = match_token t Tok_NotEqual in
              let (tb, e') = parse_expr_equality ta in
              (tb, NotEqual (e, e'))
  | _ -> t, e

and parse_expr_relational toks = 
  let (t, e) = parse_expr_additive toks in
  match lookahead t with
  | Tok_Less -> let ta = match_token t Tok_Less in
              let (tb, e') = parse_expr_relational ta in
              (tb, Less (e, e'))
  | Tok_Greater -> let ta = match_token t Tok_Greater in
              let (tb, e') = parse_expr_relational ta in
              (tb, Greater (e, e'))
  | Tok_LessEqual -> let ta = match_token t Tok_LessEqual in
              let (tb, e') = parse_expr_relational ta in
              (tb, LessEqual (e, e'))
  | Tok_GreaterEqual -> let ta = match_token t Tok_GreaterEqual in
              let (tb, e') = parse_expr_relational ta in
              (tb, GreaterEqual (e, e'))
  | _ -> t, e

and parse_expr_additive toks = 
  let (t, e) = parse_expr_multiplicative toks in
  match lookahead t with
  | Tok_Add -> let ta = match_token t Tok_Add in
              let (tb, e') = parse_expr_additive ta in
              (tb, Add (e, e'))
  | Tok_Sub -> let ta = match_token t Tok_Sub in
              let (tb, e') = parse_expr_additive ta in
              (tb, Sub (e, e'))
  | _ -> t, e

and parse_expr_multiplicative toks = 
  let (t, e) = parse_expr_power toks in
  match lookahead t with
  | Tok_Mult -> let ta = match_token t Tok_Mult in
              let (tb, e') = parse_expr_multiplicative ta in
              (tb, Mult (e, e'))
  | Tok_Div -> let ta = match_token t Tok_Div in
              let (tb, e') = parse_expr_multiplicative ta in
              (tb, Div (e, e'))
  | _ -> t, e

and parse_expr_power toks = 
  let (t, e) = parse_expr_unary toks in
  match lookahead t with
  | Tok_Pow -> let ta = match_token t Tok_Pow in
              let (tb, e') = parse_expr_power ta in
              (tb, Pow (e, e'))
  | _ -> t, e

and parse_expr_unary toks = 
  match lookahead toks with
  | Tok_Not -> let t = match_token toks Tok_Not in
              let (ta, e) = parse_expr_unary t in
              (ta, Not e)
  | _ -> parse_expr_primary toks

and parse_expr_primary toks = 
  match lookahead toks with
  | Tok_Int i -> let t = match_token toks (Tok_Int i) in
                  (t, Int i)
  | Tok_Bool b -> let t = match_token toks (Tok_Bool b) in
                  (t, Bool b)
  | Tok_ID s -> let t = match_token toks (Tok_ID s) in
                  (t, ID s)
  | Tok_LParen -> let t = match_token toks Tok_LParen in
                  let (ta, s) = parse_expr t in
                  let tb = match_token ta Tok_RParen in
                  (tb, s)
  | _ -> raise (InvalidInputException "parse_expr2")

;;
let rec parse_stmt toks : stmt_result =

  parseStmt toks

and parseStmt toks = 
  let (t, m) = pOptions toks in
  match lookahead t with
  | Tok_Int_Type -> let (ta, s) = parseStmt t in
                  (ta, Seq (m, s))
  | Tok_Bool_Type -> let (ta, s) = parseStmt t in
                  (ta, Seq (m, s))
  | Tok_ID x -> let (ta, s) = parseStmt t in
                  (ta, Seq (m, s))
  | Tok_Print -> let (ta, s) = parseStmt t in
                  (ta, Seq (m, s))
  | Tok_If -> let (ta, s) = parseStmt t in
                  (ta, Seq (m, s))
  | Tok_For -> let (ta, s) = parseStmt t in
                  (ta, Seq (m, s))
  | Tok_While -> let (ta, s) = parseStmt t in
                  (ta, Seq (m, s))
  
  | _ -> (t, Seq (m , NoOp))

and pOptions toks = 
  match lookahead toks with
  | Tok_Int_Type -> let t = match_token toks Tok_Int_Type  in
          let (b,str) = 
                    match (lookahead t) with 
                        | Tok_ID x -> (true, x) 
                        | _ -> (false, "") in 
          if b = true then  
            let ta = match_token t (Tok_ID str) in
                          let tb = match_token ta Tok_Semi in
                          (tb, Declare(Int_Type, str))
          else raise (InvalidInputException "Invalid Int ID")
                  
  | Tok_Bool_Type -> let t = match_token toks Tok_Bool_Type in
           let (b,str) =
                    match (lookahead t) with 
                        | Tok_ID x -> (true, x) 
                        | _ -> (false, "") in  
          if b = true then
            let ta = match_token t (Tok_ID str) in
                          let tb = match_token ta Tok_Semi in
                          (tb, Declare(Int_Type, str))
          else raise (InvalidInputException "Invalid Bool ID")
  | Tok_ID x -> let t = match_token toks (Tok_ID x) in
          let ta = match_token t Tok_Assign in 
          let (tb, e) = parse_expr ta in 
          let tc = match_token tb Tok_Semi in
          (tc, Assign (x,e))

  | Tok_Print -> let t = match_token toks Tok_Print in
          let ta = match_token t Tok_LParen in 
          let (tb, e) = parse_expr ta in 
          let tc = match_token tb Tok_RParen in
          let td = match_token tc Tok_Semi in
          (td, Print e)
  | Tok_If -> let t = match_token toks Tok_If in
          let ta = match_token t Tok_LParen in 
          let (tb, e) = parse_expr ta in 
          let tc = match_token tb Tok_RParen in
          let td = match_token tc Tok_LBrace in
          if lookahead td = Tok_RBrace then
            let t2a = match_token td Tok_RBrace in
            if (lookahead t2a) = Tok_Else then 
             let t2b = match_token t2a Tok_Else in 
             let t3 = match_token t2b Tok_LBrace in 
             if lookahead t3 = Tok_RBrace then
             let t3b = match_token t3 Tok_RBrace in
             (t3b, If (e,NoOp,NoOp))
             else
             let (t3a, s') = parseStmt t3 in 
             let t3b = match_token t3a Tok_RBrace in
             (t3b, If (e,NoOp,s'))
            else (t2a, If (e,NoOp,NoOp))
          else
            let (t2, s) = parseStmt td in 
            let t2a = match_token t2 Tok_RBrace in
            if (lookahead t2a) = Tok_Else then 
            let t2b = match_token t2a Tok_Else in 
            let t3 = match_token t2b Tok_LBrace in 
             if lookahead t3 = Tok_RBrace then
             let t3b = match_token t3 Tok_RBrace in
             (t3b, If (e,s,NoOp))
             else
             let (t3a, s') = parseStmt t3 in 
             let t3b = match_token t3a Tok_RBrace in
             (t3b, If (e,s,s'))
            else (t2a, If (e,s,NoOp))
            
  | Tok_For -> let t = match_token toks Tok_For in
          let ta = match_token t Tok_LParen in 
           let (b,str) = 
                    match (lookahead ta) with 
                        | Tok_ID x -> (true, x) 
                        | _ -> (false, "") in  
          if b = true then let tb = match_token ta (Tok_ID str) in
          let tc = match_token tb Tok_From in 
          let (t2, e) = parse_expr tc in
          let t2a = match_token t2 Tok_To in 
          let (t2b, e') = parse_expr t2a in
          let tz = match_token t2b Tok_RParen in 
          let t3 = match_token tz Tok_LBrace in 
          if lookahead t3 = Tok_RBrace then
          let t3a = match_token t3 Tok_RBrace in
          (t3a , For(str,e,e',NoOp))
          else
          let (t3a, s) = parseStmt t3 in 
          let t3b = match_token t3a Tok_RBrace in
          (t3b , For(str,e,e',s))

          else raise (InvalidInputException "Invalid For ID") 



  | Tok_While -> let t = match_token toks Tok_While in
          let ta = match_token t Tok_LParen in 
          let (tb, e) = parse_expr ta in 
          let tc = match_token tb Tok_RParen in
          let td = match_token tc Tok_LBrace in
          if lookahead td = Tok_RBrace then 
          let t2 = match_token td Tok_RBrace in
          (t2, While(e,NoOp))
          else
          let (t2, s) = parseStmt td in 
          let t2a = match_token t2 Tok_RBrace in
          (t2a, While(e,s))
  
  
  | _ ->  raise (InvalidInputException "Invalid stmt")



;;

let parse_main toks : stmt =
  let t = match_token toks Tok_Int_Type in
  let ta = match_token t Tok_Main in
  let tb = match_token ta Tok_LParen in
  let tc = match_token tb Tok_RParen in
  let t2 = match_token tc Tok_LBrace in
  match lookahead t2 with
  | Tok_RBrace -> let t3 = match_token t2 Tok_RBrace in
  if t3 <> [EOF]  then raise (InvalidInputException "Invalid Main")
  else NoOp 
  | _ -> let (t2a, s) = parse_stmt t2 in 
  let t3 = match_token t2a Tok_RBrace in
  
  if t3 <> [EOF]  then raise (InvalidInputException "Invalid Main")
  else s 
  ;;
  
  
  
  
  
  
  
  
  
  
  
  

