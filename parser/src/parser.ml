open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* PASTE YOUR PARSERS FROM P4A HERE *)

let rec parse_expr toks = parse_Expr toks

and parse_Expr toks =
    match lookahead toks with
    | Some (Tok_Let) -> let (t, l) = parse_LetExpr toks in (t, l)
    | Some (Tok_If) -> let (t, i) = parse_IfExpr toks in (t, i)
    | Some (Tok_Fun) -> let (t, f) = parse_FunctionExpr toks in (t, f)
    | _ -> let (t, o) = parse_OrExpr toks in (t, o)

and parse_LetExpr toks =
    match lookahead toks with
    | Some (Tok_Let) -> (let t' = match_token toks Tok_Let in
                 match (lookahead t'), (lookahead_many t' 1), (lookahead_many t' 2) with
                 | (Some (Tok_Rec)), (Some (Tok_ID (str_id))), (Some (Tok_Equal)) -> let (t'', e1) = parse_Expr (match_many t' [Tok_Rec ; (Tok_ID (str_id)) ; Tok_Equal]) in
                                                                               (match lookahead t'' with
                                                                               | Some (Tok_In) -> let (t''', e2) = parse_Expr (match_token t'' Tok_In) in
                                                                                           (t''', Let (str_id, true, e1, e2))
                                                                               | _ -> raise (InvalidInputException "invalid input"))
                 (* why does this match as a 3-tuple ?? *)
                 | (Some (Tok_ID (str_id))), (Some (Tok_Equal)), (_) -> let (t'', e1) = parse_Expr (match_many t' [(Tok_ID (str_id)) ; Tok_Equal]) in
                                                         (match lookahead t'' with
                                                         | Some (Tok_In) -> let (t''', e2) = parse_Expr (match_token t'' Tok_In) in
                                                                     (t''', Let (str_id, false, e1, e2))
                                                         | _ -> raise (InvalidInputException "invalid input"))
                 | _, _, _ -> raise (InvalidInputException "invalid input"))
    | _ -> raise (InvalidInputException "invalid input")

and parse_FunctionExpr toks =
    match (lookahead toks), (lookahead_many toks 1), (lookahead_many toks 2) with
    | Some (Tok_Fun), Some (Tok_ID (str_id)), Some (Tok_Arrow) -> let (t', e) = parse_Expr (match_many toks [(Tok_Fun) ; (Tok_ID (str_id)) ; (Tok_Arrow)]) in
                                                                  (t', Fun (str_id, e))
    | _, _, _ -> raise (InvalidInputException "invalid input")

and parse_IfExpr toks =
    match (lookahead toks) with
    | Some (Tok_If) -> let (t', e1) = parse_Expr (match_token toks Tok_If) in
                       (match lookahead t' with
                       | Some (Tok_Then) -> let (t'', e2) = parse_Expr (match_token t' Tok_Then) in
                                            (match lookahead t'' with
                                            | Some (Tok_Else) -> let (t''', e3) = parse_Expr (match_token t'' Tok_Else) in
                                                                 (t''', If (e1, e2, e3))
                                            | _ -> raise (InvalidInputException "invalid input"))
                       | _ -> raise (InvalidInputException "invalid input"))
    | _ -> raise (InvalidInputException "invalid input")

and parse_OrExpr toks =
    let (t', e1) = parse_AndExpr (toks) in
    match lookahead t' with
    | Some (Tok_Or) -> let (t'', e2) = parse_OrExpr (match_token t' Tok_Or) in (t'', Binop (Or, e1, e2))
    | _ -> (t', e1)

and parse_AndExpr toks =
    let (t', e1) = parse_EqualityExpr toks in
    match lookahead t' with
    | Some (Tok_And) -> let (t'', e2) = parse_AndExpr (match_token t' Tok_And) in (t'', Binop (And, e1, e2))
    | _ -> (t', e1)

and parse_EqualityExpr toks =
    let (t', e1) = parse_RelationalExpr toks in
    match lookahead t' with
    | Some (Tok_Equal) -> let (t'', e2) = parse_EqualityExpr (match_token t' Tok_Equal) in (t'', Binop (Equal, e1, e2))
    | Some (Tok_NotEqual) -> let (t'', e2) = parse_EqualityExpr (match_token t' Tok_NotEqual) in (t'', Binop (NotEqual, e1, e2))
    | _ -> (t', e1)

and parse_RelationalExpr toks =
    let (t', e1) = parse_AdditiveExpr toks in
    match lookahead t' with
    | Some (Tok_Greater) -> let (t'', e2) = parse_RelationalExpr (match_token t' Tok_Greater) in (t'', Binop (Greater, e1, e2))
    | Some (Tok_Less) -> let (t'', e2) = parse_RelationalExpr (match_token t' Tok_Less) in (t'', Binop (Less, e1, e2))
    | Some (Tok_GreaterEqual) -> let (t'', e2) = parse_RelationalExpr (match_token t' Tok_GreaterEqual) in (t'', Binop (GreaterEqual, e1, e2))
    | Some (Tok_LessEqual) -> let (t'', e2) = parse_RelationalExpr (match_token t' Tok_LessEqual) in (t'', Binop (LessEqual, e1, e2))
    | _ -> (t', e1)

and parse_AdditiveExpr toks =
    let (t', e1) = parse_MultiplicativeExpr toks in
    match lookahead t' with
    | Some (Tok_Add) -> let (t'', e2) = parse_AdditiveExpr (match_token t' Tok_Add) in (t'', Binop (Add, e1, e2))
    | Some (Tok_Sub) -> let (t'', e2) = parse_AdditiveExpr (match_token t' Tok_Sub) in (t'', Binop (Sub, e1, e2))
    | _ -> (t', e1)

and parse_MultiplicativeExpr toks =
    let (t', e1) = parse_ConcatExpr toks in
    match lookahead t' with
    | Some (Tok_Mult) -> let (t'', e2) = parse_MultiplicativeExpr (match_token t' Tok_Mult) in (t'', Binop (Mult, e1, e2))
    | Some (Tok_Div) -> let (t'', e2) = parse_MultiplicativeExpr (match_token t' Tok_Div) in (t'', Binop (Div, e1, e2))
    | _ -> (t', e1)

and parse_ConcatExpr toks =
     let (t', e1) = parse_UnaryExpr toks in
     match lookahead t' with
     | Some (Tok_Concat) -> let (t'', e2) = parse_ConcatExpr (match_token t' Tok_Concat) in (t'', Binop (Concat, e1, e2))
     | _ -> (t', e1)

and parse_UnaryExpr toks =
    match lookahead toks with
    | Some (Tok_Not) -> let (t', e1) = parse_UnaryExpr (match_token toks Tok_Not) in (t', Not (e1))
    | _ -> parse_FunctionCallExpr (toks)

and parse_FunctionCallExpr toks =
    let (t', e1) = parse_PrimaryExpr toks in
    match lookahead t' with
    | (Some (Tok_Int (i))) -> let (t'', e2) = parse_PrimaryExpr (t') in (t'', FunctionCall (e1, e2))
    | (Some (Tok_Bool (b))) -> let (t'', e2) = parse_PrimaryExpr (t') in (t'', FunctionCall (e1, e2))
    | (Some (Tok_String (s))) -> let (t'', e2) = parse_PrimaryExpr (t') in (t'', FunctionCall (e1, e2))
    | (Some (Tok_ID (id))) -> let (t'', e2) = parse_PrimaryExpr (t') in (t'', FunctionCall (e1, e2))
    | (Some (Tok_LParen)) -> let (t'', e2) = parse_PrimaryExpr (t') in (t'', FunctionCall (e1, e2))
    | _ -> (t', e1)

and parse_PrimaryExpr toks =
    match lookahead toks with
    | (Some (Tok_Int (i))) -> ((match_token toks (Tok_Int (i))), (Value (Int (i))))
    | (Some (Tok_Bool (b))) -> ((match_token toks (Tok_Bool (b))), (Value (Bool (b))))
    | (Some (Tok_String (s))) -> ((match_token toks (Tok_String (s))), (Value (String (s))))
    | (Some (Tok_ID (id))) -> ((match_token toks (Tok_ID (id))), (ID (id)))
    | (Some (Tok_LParen)) -> let (t', e1) = parse_Expr (match_token toks Tok_LParen) in
                             (match lookahead t' with
                              | (Some (Tok_RParen)) -> ((match_token t' Tok_RParen), e1)
                              | _ -> raise (InvalidInputException "invalid input"))
    | _ -> raise (InvalidInputException "invalid input")


(* Part 3: Parsing mutop *)

let rec parse_mutop toks =
    match lookahead toks with
    | Some (Tok_DoubleSemi) -> ((match_token toks Tok_DoubleSemi), NoOp)
    | Some (Tok_Def) -> (match (lookahead_many toks 1), (lookahead_many toks 2) with
                        | (Some (Tok_ID (id))), (Some (Tok_Equal)) -> let (t', e1) = parse_Expr (match_many toks [Tok_Def ; (Tok_ID (id)); Tok_Equal]) in
                                                                      (match lookahead t' with
                                                                      | Some (Tok_DoubleSemi) -> ((match_token t' Tok_DoubleSemi), Def (id, e1))
                                                                      | _ -> raise (InvalidInputException "invalid input"))
                        | _ -> raise (InvalidInputException "invalid input"))
    | _ -> let (t', e1) = parse_Expr (toks) in
           (match lookahead t' with
           | Some (Tok_DoubleSemi) -> ((match_token t' Tok_DoubleSemi), Expr (e1))
           | _ -> raise (InvalidInputException "invalid input"))