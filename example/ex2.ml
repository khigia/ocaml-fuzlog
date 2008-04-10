open Fuzlog
open Inference
open Builder

let vocabulary =
    (* define few liguistic variables and register them *)
    let voc = Vocabulary.create () in
    let _ = Vocabulary.set
        voc
        "BIG"
        (FuzzySet.create_triangle 5.0 15.0)
    in
    let _ = Vocabulary.set
        voc
        "FAST"
        (FuzzySet.create_triangle 90.0 110.0)
    in
    voc
    (* TODO complete the parser for vocabulary
    Builder.vocabulary_from_string "
        DEF BIG  TRIANGLE 5.0 15.0
        DEF FAST TRIANGLE 90 110
    "
    *)

let create_model voc =
    let rules = Builder.rules_from_string voc "
        IF input1 IS BIG AND input2 IS BIG THEN ou1 IS FAST
        IF input2 IS BIG THEN ou2 IS FAST
        IF input2 IS BIG THEN ou1 IS FAST
    " in
    Controller.create rules


let eval_model ctrl =
    let fuzIO = FuzIO.create () in
    (* set some inputs *)
    FuzIO.set_input fuzIO "input1" 6.0;
    FuzIO.set_input fuzIO "input2" 7.0;
    (* eval the fuzzy inference *)
    (* TODO error case: input not found *)
    ignore(Controller.eval ctrl fuzIO);
    (* print some output after defuzzification *)
    FuzIO.debug fuzIO;
    List.iter
        (fun (name, value) ->
            Printf.printf "Defuzz: %s=%f\n" name value
        )
        (Controller.defuzz ctrl fuzIO)

let _ = eval_model (create_model vocabulary)
