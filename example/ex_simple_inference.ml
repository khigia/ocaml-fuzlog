open Fuzlog
open Inference

let create_model () =
    (* define few liguistic variables *)
    let big = VarLing.create
        "BIG"
        (FuzzySet.create_triangle 5.0 15.0)
    in
    let fast = VarLing.create
        "FAST"
        (FuzzySet.create_triangle 90.0 110.0)
    in
    (* a model is a set of functions and a set of rules *)
    Controller.create
        Norm.minimum (* how to compute "a AND b" in rule premisse *)
        Implication.larsen (* definition of implication operator *)
        IsAlso.maximum (* define "output IS V1 AND output IS V2" in conclusion *)
        Defuzzyfication.barycenter (* from fuzzy value to crisp one *)
        [
            (* Rule1: IF in1 IS big AND in2 IS BIG THEN ou1 IS fast *)
            (
                (Premisse.connect_and
                    (Premisse.create_input "input1" big)
                    (Premisse.create_input "input2" big)
                ),
                (Conclusion.create_output "output1" fast)
            );
            (* Rule2: IF in2 IS BIG THEN ou2 IS fast *)
            (
                (Premisse.create_input "input2" big),
                (Conclusion.create_output "output2" fast)
            );
            (* Rule2: IF in2 IS BIG THEN ou1 IS fast *)
            (
                (Premisse.create_input "input2" big),
                (Conclusion.create_output "output1" fast)
            );
        ]

let doit () = 
    (* model is the fuzzy controller *)
    let ctrl1 = create_model () in
    (* all input/output to model are set/get through FuzIO:
    this is to enable to use the same model for multiple control state
    *)
    let fuzIO = FuzIO.create () in
    (* set some inputs *)
    FuzIO.clear fuzIO;
    FuzIO.set_input fuzIO "input1" 6.0;
    FuzIO.set_input fuzIO "input2" 7.0;
    (* eval the fuzzy inference *)
    ignore(Controller.eval ctrl1 fuzIO);
    FuzIO.debug fuzIO;
    (* get some output after defuzzification *)
    List.iter
        (fun (name, value) ->
            Printf.printf "Defuzz: %s=%f\n" name value
        )
        (Controller.defuzz ctrl1 fuzIO)
    
let _ = doit ()
