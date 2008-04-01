type symb_t =
    | Symb of string

type is_t =
    | Is of symb_t * symb_t
    | And of is_t * is_t

type rule_t = 
    | Imply of is_t * is_t


let indented_string indent s =
    Printf.sprintf "%s%s" (String.make indent ' ') s

let indented_binop indent op left right =
    Printf.sprintf "%s%s\n%s\n%s"
        (String.make indent ' ')
        op
        left
        right

let to_s_tree_symb t indent = match t with
    | Symb s ->
        indented_string indent s

let rec to_s_tree_is t indent = match t with
    | Is(s1, s2) ->
        indented_binop
            indent
            "IS"
            (to_s_tree_symb s1 (indent + 1))
            (to_s_tree_symb s2 (indent + 1))
    | And(s1, s2) ->
        indented_binop
            indent
            "AND"
            (to_s_tree_is s1 (indent + 1))
            (to_s_tree_is s2 (indent + 1))

let rec to_s_tree t indent = match t with
    | Imply(s1, s2) ->
        indented_binop
            indent
            "IF-THEN"
            (to_s_tree_is s1 (indent + 1))
            (to_s_tree_is s2 (indent + 1))


