open Fuzlog
open Inference

let model =
    (* let's define some fuzzy values *)
    let big = Var.create "BIG" (FuzzySet.create_triangle 5.0 15.0) in
    let fast = Var.create "FAST" (FuzzySet.create_triangle 90.0 110.0) in
    (* now come the model (aka controller) *)
    Controller.create
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
            (* Rule3: IF in2 IS BIG THEN ou1 IS fast *)
            (Rule.create 
                (Premisse.create_input "input2" big)
                (Conclusion.create_output "output1" fast)
            );
        ]

let eval ctrl =
    let fuzIO = FuzIO.create () in
    (* set some inputs *)
    FuzIO.set_input fuzIO "input1" 6.0;
    FuzIO.set_input fuzIO "input2" 7.0;
    (* eval the fuzzy inference *)
    (* TODO error case: input not found *)
    ignore(Controller.eval ctrl fuzIO);
    (* print some outputs *)
    FuzIO.debug fuzIO;
    List.iter
        (fun (name, value) ->
            Printf.printf "Defuzz: %s=%f\n" name value
        )
        (Controller.defuzz ctrl fuzIO)

let _ = eval model
