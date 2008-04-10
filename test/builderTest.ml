open Fuzlog
open Inference
open Ast

let _ = Tests.register "Build AST for vocabulary" (fun () ->
    let nodes = Builder.voc_ast_from_string "
        DEF BIG  TRIANGLE 5.0 15.0
        DEF SMURGL [ (1 0.1) (2 0.4) ]
    " in
    ignore(OUnit.assert_equal 2 (List.length nodes));
    let result = match nodes with
        |
            (DefByFun(Symb "BIG", Symb "TRIANGLE", [5.0; 15.0;]))
            ::
            (DefByPoints(Symb "SMURGL", [(1.0, 0.1); (2.0, 0.4);]))
            ::
            []
            -> true
        | _ -> false
    in
    OUnit.assert_bool "Not expected AST for vocabulary" result
)

let _ = Tests.register "Build AST for rules" (fun () ->
    let nodes = Builder.rule_ast_from_string "
        IF in10 IS big THEN ou10 IS fast

        IF
            in20_1 IS big
            AND in20_2 IS big
        THEN
            ou20 IS fast
        
        IF in25_1 IS big
           AND in25_2 IS big
           AND in25_3 IS small
        THEN ou25 IS fast
        
    " in
    ignore(OUnit.assert_equal 3 (List.length nodes));
    let result = match nodes with
        |
            (Imply(Is(Symb _, Symb _), Is(Symb _, Symb _)))
            ::
            (Imply(And(Is(Symb _, Symb _),Is(Symb _, Symb _)), Is(Symb _, Symb _)))
            ::
            (Imply(And(And(Is(Symb _, Symb _),Is(Symb _, Symb _)),Is(Symb _, Symb _)), Is(Symb _, Symb _)))
            ::
            []
            -> true
        | _ -> false
    in
    OUnit.assert_bool "Not expected AST for rules" result
)

let _ = Tests.register "Build rules" (fun () ->
    let voc = Vocabulary.create () in
    let _ = Vocabulary.set voc "big" (FuzzySet.create_triangle 1.0 3.0) in
    let _ = Vocabulary.set voc "fast" (FuzzySet.create_triangle 10.0 30.0) in
    let rules = Builder.rules_from_string voc "
        IF in10 IS big THEN ou10 IS fast
    " in
    OUnit.assert_equal 1 (List.length rules)
)

let _ = Tests.run "Builder (parser/ast) test suite"
