open Fuzlog
open Inference

let doit () = 
    let fuzIO = FuzIO.create () in
    let big = VarLing.create
        "BIG"
        (FuzzySet.create_triangle 5.0 15.0)
    in
    let fast = VarLing.create
        "FAST"
        (FuzzySet.create_triangle 90.0 110.0)
    in
    let ctrl1 = Controller.create
        Norm.minimum
        Implication.larsen
        IsAlso.maximum
        Defuzzyfication.barycenter
        [
            (
                (Premisse.connect_and
                    (Premisse.create_input "input1" big)
                    (Premisse.create_input "input2" big)
                ),
                (Conclusion.create_output "output1" fast)
            );
            (
                (Premisse.create_input "input2" big),
                (Conclusion.create_output "output2" fast)
            );
            (
                (Premisse.create_input "input2" big),
                (Conclusion.create_output "output1" fast)
            );
        ]
    in
    FuzIO.clear fuzIO;
    FuzIO.set_input fuzIO "input1" 6.0;
    FuzIO.set_input fuzIO "input2" 7.0;
    ignore(Controller.eval ctrl1 fuzIO);
    FuzIO.debug fuzIO;
    List.iter
        (fun (name, value) ->
            Printf.printf "Defuzz: %s=%f\n" name value
        )
        (Controller.defuzz ctrl1 fuzIO)
    
let _ = doit ()
