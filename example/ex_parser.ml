open Fuzlog

(* TODO have a look to tutorial ... remember good practices for error management http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/
*)
let doit () =
    (* TODO looks like utests :) *)
    let lexbuf = Lexing.from_string "
        IF in10 IS big THEN ou10 IS fast
        IF in20_1 IS big AND in20_2 IS big THEN ou20 IS fast
        IF in25_1 IS big AND in25_2 IS big AND in25_3 IS small THEN ou25 IS fast
        IF in30 IS big THEN ou30_1 IS fast AND ou30_2 IS slow
    " in
    let ruleTrees = Parser.rules Lexer.tokens lexbuf in
    List.fold_left
        (fun num rTree -> Printf.printf "Rule tree %d:\n%s\n" num (Ast.to_s_tree rTree 2); num + 1)
        0
        ruleTrees

let _ = doit ()
