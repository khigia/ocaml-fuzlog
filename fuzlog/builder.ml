open Ast
open Inference

let to_symb t = match t with
    | Symb s -> s

let to_var voc t = match t with
    (* TODO error case: var not found *)
    | Symb s ->
        Vocabulary.get voc s

let rec to_premisse voc t = match t with
    | Is(s1, s2) ->
        Premisse.create_input (to_symb s1) (to_var voc s2)
    | And(s1, s2) ->
        Premisse.connect_and
            (to_premisse voc s1)
            (to_premisse voc s2)

let to_conclusion voc t = match t with
    | Is(s1, s2) ->
        Conclusion.create_output (to_symb s1) (to_var voc s2)
    | And(s1, s2) ->
        failwith "not implemented: rules can have only one conclusion :("

let to_rule voc t = match t with
    | Imply(s1, s2) ->
        Rule.create
            (to_premisse voc s1)
            (to_conclusion voc s2)


let rule_ast_from_string s =
    let lexbuf = Lexing.from_string s in
    Parser.rules Lexer.tokens lexbuf

let rules_from_string voc s =
    List.map (to_rule voc) (rule_ast_from_string s)

