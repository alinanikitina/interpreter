open TokenTypes

(* PASTE YOUR LEXER FROM P4A HERE *)

let tokenize input =
    let rec tok pos =
        if pos >= String.length input then []
        else if Str.string_match (Str.regexp "true\\|false") input pos then
                let token = Str.matched_string input in
                    if token = "true" then (Tok_Bool (true))::(tok (pos + 4))
                    else (Tok_Bool (false))::(tok (pos + 5))
        else if Str.string_match (Str.regexp "(-[0-9]+)") input pos then
                let token = Str.matched_string input in
                    (Tok_Int (int_of_string (String.sub token 1 ((String.length token) - 2))))::(tok (pos + (String.length token)))
        else if Str.string_match (Str.regexp "[0-9]+") input pos then
                let token = Str.matched_string input in
                    (Tok_Int (int_of_string (token)))::(tok (pos + (String.length token)))
        else if Str.string_match (Str.regexp "\"[^\"]*\"") input pos then
                let token = Str.matched_string input in
                     (Tok_String (String.sub token 1 ((String.length token) - 2)))::(tok (pos + (String.length token)))
        else if (Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos) then
                let token = Str.matched_string input in
                    if (token = "let") then (Tok_Let)::(tok (pos + 3))
                    else if (token = "rec") then (Tok_Rec)::(tok (pos + 3))
                    else if (token = "in") then (Tok_In)::(tok (pos + 2))
                    else if (token = "def") then (Tok_Def)::(tok (pos + 3))
                    else if (token = "fun") then (Tok_Fun)::(tok (pos + 3))
                    else if (token = "not") then (Tok_Not)::(tok (pos + 3))
                    else if (token = "if") then (Tok_If)::(tok (pos + 2))
                    else if (token = "then") then (Tok_Then)::(tok (pos + 4))
                    else if (token = "else") then (Tok_Else)::(tok (pos + 4))
                    else (Tok_ID (token))::(tok (pos + (String.length token)))
        else if Str.string_match (Str.regexp ")") input pos then
                (Tok_RParen)::(tok (pos + 1))
        else if Str.string_match (Str.regexp "(") input pos then
                (Tok_LParen)::(tok (pos + 1))
        else if Str.string_match (Str.regexp "=") input pos then
                (Tok_Equal)::(tok (pos + 1))
        else if Str.string_match (Str.regexp "<>") input pos then
                (Tok_NotEqual)::(tok (pos + 2))
        else if Str.string_match (Str.regexp "->") input pos then
                (Tok_Arrow)::(tok (pos + 2))
        else if Str.string_match (Str.regexp ">=") input pos then
                (Tok_GreaterEqual)::(tok (pos + 2))
        else if Str.string_match (Str.regexp "<=") input pos then
                (Tok_LessEqual)::(tok (pos + 2))
        else if Str.string_match (Str.regexp ">") input pos then
                (Tok_Greater)::(tok (pos + 1))
        else if Str.string_match (Str.regexp "<") input pos then
                (Tok_Less)::(tok (pos + 1))
        else if Str.string_match (Str.regexp "&&") input pos then
                (Tok_And)::(tok (pos + 2))
        else if Str.string_match (Str.regexp "||") input pos then
                (Tok_Or)::(tok (pos + 2))
        else if Str.string_match (Str.regexp "not") input pos then
                (Tok_Not)::(tok (pos + 3))
        else if Str.string_match (Str.regexp "if") input pos then
                (Tok_If)::(tok (pos + 2))
        else if Str.string_match (Str.regexp "then") input pos then
                (Tok_Then)::(tok (pos + 4))
        else if Str.string_match (Str.regexp "else") input pos then
                (Tok_Else)::(tok (pos + 4))
        else if Str.string_match (Str.regexp "\\*") input pos then
                (Tok_Mult)::(tok (pos + 1))
        else if Str.string_match (Str.regexp "/") input pos then
                (Tok_Div)::(tok (pos + 1))
        else if Str.string_match (Str.regexp "\\+") input pos then
                (Tok_Add)::(tok (pos + 1))
        else if Str.string_match (Str.regexp "-") input pos then
                (Tok_Sub)::(tok (pos + 1))
        else if Str.string_match (Str.regexp "\\^") input pos then
                (Tok_Concat)::(tok (pos + 1))
        else if Str.string_match (Str.regexp "let") input pos then
                (Tok_Let)::(tok (pos + 3))
        else if Str.string_match (Str.regexp "rec") input pos then
                (Tok_Rec)::(tok (pos + 3))
        else if Str.string_match (Str.regexp "in") input pos then
                (Tok_In)::(tok (pos + 2))
        else if Str.string_match (Str.regexp "def") input pos then
                (Tok_Def)::(tok (pos + 3))
        else if Str.string_match (Str.regexp "fun") input pos then
                (Tok_Fun)::(tok (pos + 3))
        else if Str.string_match (Str.regexp ";;") input pos then
                (Tok_DoubleSemi)::(tok (pos + 2))
        else
                tok (pos + 1)
    in tok 0;;