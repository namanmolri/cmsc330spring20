open TokenTypes
open String

let reg_for = Str.regexp "for[(| ]"
let reg_from = Str.regexp "from[(| ]"
let reg_to = Str.regexp "to "
let reg_while = Str.regexp "while[(| ]" 
let reg_int_type = Str.regexp "int "
let reg_bool_type =Str.regexp "bool "
let reg_sub = Str.regexp "-"
let reg_semi = Str.regexp ";"
let reg_rparen = Str.regexp ")"
let reg_rbrace = Str.regexp "}"
let reg_print =Str.regexp "printf[(| ]"
let reg_pow = Str.regexp "\\^"
let reg_add = Str.regexp "+"
let reg_or = Str.regexp "||"
let reg_ne = Str.regexp "!="
let reg_not =Str.regexp "!"
let reg_mult = Str.regexp "\\*"
let reg_main = Str.regexp "main[(| ]"
let reg_le = Str.regexp "<="
let reg_less = Str.regexp "<"
let reg_lparen = Str.regexp "("
let reg_lbrace = Str.regexp "{"
let reg_int = Str.regexp "-?[0-9]+"
let reg_if = Str.regexp "if"
let reg_str = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let reg_ge = Str.regexp ">="
let reg_greater = Str.regexp ">"
let reg_eq = Str.regexp "=="
let reg_else =Str.regexp "else[{| ]"
let reg_div = Str.regexp "/"
let reg_bool = Str.regexp "(true|false)"
let reg_assign = Str.regexp "="
let reg_and= Str.regexp "&&"




let tokenize input =

  let rec tok stringPos t =
    if (stringPos >= String.length t) then
        [EOF]
    
    else if (Str.string_match (Str.regexp " ") t stringPos) then
            (tok (stringPos+1)) t 
    
    else if (Str.string_match (Str.regexp "\n") t stringPos) then
            (tok (stringPos+1)) t 
    
    else if (Str.string_match (Str.regexp "\t") t stringPos) then
            (tok (stringPos+1)) t 

    else if (Str.string_match reg_for t stringPos) then
            Tok_For::(tok (stringPos+3)) t

    else if (Str.string_match reg_from t stringPos) then
            Tok_From::(tok (stringPos+4)) t
        
    else if (Str.string_match reg_to t stringPos) then
            Tok_To::(tok (stringPos+2)) t

    else if (Str.string_match reg_while t stringPos) then
            Tok_While::(tok (stringPos+5)) t

    else if (Str.string_match reg_semi t stringPos) then
            Tok_Semi::(tok (stringPos+1)) t

    else if (Str.string_match reg_rparen t stringPos) then
            Tok_RParen::(tok (stringPos+1)) t

    else if (Str.string_match reg_rbrace t stringPos) then
            Tok_RBrace::(tok (stringPos+1)) t

    else if (Str.string_match reg_print t stringPos) then
           Tok_Print::(tok (stringPos+6)) t

    else if (Str.string_match reg_pow t stringPos) then
            Tok_Pow::(tok (stringPos+1)) t

    else if (Str.string_match reg_add t stringPos) then
            Tok_Add::(tok (stringPos+1)) t

    else if (Str.string_match reg_or t stringPos) then
            Tok_Or::(tok (stringPos+2)) t

    else if (Str.string_match reg_ne t stringPos) then
            Tok_NotEqual::(tok (stringPos+2)) t

    else if (Str.string_match reg_not t stringPos) then
            Tok_Not::(tok (stringPos+1)) t

    else if (Str.string_match reg_mult t stringPos) then
            Tok_Mult::(tok (stringPos+1)) t

    else if (Str.string_match reg_main t stringPos) then
            Tok_Main::(tok (stringPos+4)) t
        
    else if (Str.string_match reg_le t stringPos) then
            Tok_LessEqual::(tok (stringPos+2)) t
        
    else if (Str.string_match reg_less t stringPos) then
            Tok_Less::(tok (stringPos+1)) t
        
    else if (Str.string_match reg_lparen t stringPos) then
            Tok_LParen::(tok (stringPos+1)) t
        
    else if (Str.string_match reg_lbrace t stringPos) then
            Tok_LBrace::(tok (stringPos+1)) t
        
    else if (Str.string_match reg_if t stringPos) then
            Tok_If::(tok (stringPos+2)) t
        
    else if (Str.string_match reg_ge t stringPos) then
            Tok_GreaterEqual::(tok (stringPos+2)) t
        
     else if (Str.string_match reg_greater t stringPos) then
            Tok_Greater::(tok (stringPos+1)) t

    else if (Str.string_match reg_eq t stringPos) then
            Tok_Equal::(tok (stringPos+2)) t
            
    else if (Str.string_match reg_else t stringPos) then
            Tok_Else::(tok (stringPos+4)) t
        
    else if (Str.string_match reg_div t stringPos) then
            Tok_Div::(tok (stringPos+1)) t
        
    else if (Str.string_match reg_assign t stringPos) then
            Tok_Assign::(tok (stringPos+1)) t
        
    else if (Str.string_match reg_and t stringPos) then
            Tok_And::(tok (stringPos+2)) t

    else if (Str.string_match reg_bool_type t stringPos) then
            Tok_Bool_Type::(tok (stringPos+4)) t

    else if (Str.string_match reg_int_type t stringPos) then
            Tok_Int_Type::(tok (stringPos+3)) t
        
    else if (Str.string_match reg_bool t stringPos) then
        let stringBool = Str.matched_string t in
              if (stringBool = "true") then 
        (Tok_Bool true)::(tok (stringPos+4)) t
                else
        (Tok_Bool false)::(tok (stringPos+5)) t
        
    else if (Str.string_match reg_str t stringPos) then
        let stringRegular = Str.matched_string t in 
        (Tok_ID stringRegular)::(tok (stringPos+(String.length stringRegular))) t
        
    else if (Str.string_match reg_int t stringPos) then
        let stringInt = Str.matched_string t in 
        (Tok_Int (int_of_string stringInt))::(tok (stringPos+(String.length stringInt))) t
        
    else if (Str.string_match reg_sub t stringPos) then
        Tok_Sub::(tok (stringPos+1)) t

    else raise (InvalidInputException "lexer")
        

    in 
        tok 0 input 
