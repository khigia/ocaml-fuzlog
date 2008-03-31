type symb_t =
    | Symb of string

type is_t =
    | Is of symb_t * symb_t
    | And of is_t * is_t

type rule_t = 
    | Imply of is_t * is_t


let rec to_s_tree_symb t indent = match t with
    | Symb s ->
        Printf.sprintf "%s%s"
            (String.make indent ' ')
            s

let rec to_s_tree_is t indent = match t with
    | Is(s1, s2) ->
        Printf.sprintf "%sIS\n%s\n%s"
            (String.make indent ' ')
            (to_s_tree_symb s1 (indent + 1))
            (to_s_tree_symb s2 (indent + 1))
    | And(s1, s2) ->
        Printf.sprintf "%sAND\n%s\n%s"
            (String.make indent ' ')
            (to_s_tree_is s1 (indent + 1))
            (to_s_tree_is s2 (indent + 1))

let rec to_s_tree t indent = match t with
    | Imply(s1, s2) ->
        Printf.sprintf "%sIF\n%s\n%sTHEN\n%s"
            (String.make indent ' ')
            (to_s_tree_is s1 (indent + 1))
            (String.make indent ' ')
            (to_s_tree_is s2 (indent + 1))


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

let rec to_conclusion voc t = match t with
    | Is(s1, s2) ->
        Conclusion.create_output (to_symb s1) (to_var voc s2)
    | And(s1, s2) ->
        failwith "not implemented: rules can have only one conclusion :("

let rec to_rule voc t = match t with
    | Imply(s1, s2) ->
        Rule.create
            (to_premisse voc s1)
            (to_conclusion voc s2)
