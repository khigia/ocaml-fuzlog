open Fuzlog
open Inference

let create_vocabulary () =
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

let create_model voc =
    (* a model is a set of functions and a set of rules *)
    (*TODO set of function should be in another module *)

    (* this is the "hand-written" code *)
    let big = Vocabulary.get voc "BIG" in
    let fast = Vocabulary.get voc "FAST" in
    Controller.create
        (* TODO using default values and setter give a more
        friendly API *)
        Norm.minimum (* how to compute "a AND b" in rule premisse *)
        Implication.larsen (* definition of implication operator *)
        IsAlso.maximum (* define "output IS V1 AND output IS V2" in conclusion *)
        Defuzzyfication.barycenter (* from fuzzy value to crisp one *)
        [
            (* Rule1: IF in1 IS big AND in2 IS BIG THEN ou1 IS fast *)
            (Rule.create 
                (Premisse.connect_and
                    (Premisse.create_input "input1" big)
                    (Premisse.create_input "input2" big)
                )
                (Conclusion.create_output "output1" fast)
            );
            (* Rule2: IF in2 IS BIG THEN ou2 IS fast *)
            (Rule.create 
                (Premisse.create_input "input2" big)
                (Conclusion.create_output "output2" fast)
            );
            (* Rule2: IF in2 IS BIG THEN ou1 IS fast *)
            (Rule.create 
                (Premisse.create_input "input2" big)
                (Conclusion.create_output "output1" fast)
            );
        ]

        (* TODO what we should write using the parser:
        let rules = Parser.parse_rules voc "
            IF in1 IS big AND in2 IS BIG THEN ou1 IS fast
            IF in2 IS BIG THEN ou2 IS fast
            IF in2 IS BIG THEN ou1 IS fast
        " in
        let ctrl = Controller.make rules
        *)

let doit () = 
    (* define the linguistic variables *)
    let voc = create_vocabulary () in
    (* define the model i.e. the fuzzy controller *)
    let ctrl1 = create_model voc in
    (* all input/output to model are set/get through FuzIO:
    this is to enable to use the same model for multiple control state
    *)
    let fuzIO = FuzIO.create () in
    (* set some inputs *)
    FuzIO.set_input fuzIO "input1" 6.0;
    FuzIO.set_input fuzIO "input2" 7.0;
    (* eval the fuzzy inference *)
    ignore(Controller.eval ctrl1 fuzIO);
    FuzIO.debug fuzIO;
    (* print some output after defuzzification *)
    List.iter
        (fun (name, value) ->
            Printf.printf "Defuzz: %s=%f\n" name value
        )
        (Controller.defuzz ctrl1 fuzIO)
    
let _ = doit ()
