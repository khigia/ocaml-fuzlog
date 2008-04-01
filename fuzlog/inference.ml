module FuzIO = struct

    type t = {
        inputs:  (string, float) Hashtbl.t;
        outputs: (string, FuzzySet.t option) Hashtbl.t;
    }

    let create () = {
        inputs = Hashtbl.create 10;
        outputs = Hashtbl.create 10;
    }

    let clear ctx =
        Hashtbl.clear ctx.inputs;
        Hashtbl.clear ctx.outputs

    let get_input ctx name =
        Hashtbl.find ctx.inputs name

    let set_input ctx name value =
        Hashtbl.replace ctx.inputs name value

    let set_output ctx name value =
        Hashtbl.replace ctx.outputs name (Some value)
    
    let apply_output ctx name f =
        let v0 = try Hashtbl.find ctx.outputs name with Not_found -> None in
        let v1 = f v0 in
        Hashtbl.replace ctx.outputs name v1

    let apply_outputs ctx f =
        Hashtbl.iter
            (fun name value -> Hashtbl.replace ctx.outputs name (f value))
            ctx.outputs

    let fold_outputs ctx f acc =
        Hashtbl.fold
            f
            ctx.outputs
            acc

    let debug ctx =
        Printf.printf "Fuzzy IO context:\n";
        Printf.printf "  Inputs:\n";
        Hashtbl.iter
            (Printf.printf "    %s=%f\n")
            ctx.inputs;
        Printf.printf "  Outputs:\n";
        Hashtbl.iter
            (fun name value -> Printf.printf
                "    %s=%s\n"
                name
                (match value with
                    | Some set -> FuzzySet.to_s set
                    | None -> "None"
                )
            )
            ctx.outputs

end (* module FuzIO *)


module Var = struct
    
    type t = {
        name: string;
        set: FuzzySet.t;
    }

    let create name set = {
        name = name;
        set = set;
    }

    let name var = var.name

    let value var = var.set

end (* module Var *)


module Vocabulary = struct
    
    type t = {
        vars: (string, Var.t) Hashtbl.t;
    }

    let create () = {
        vars = Hashtbl.create 10;
    }
    
    let get voc symb =
        Hashtbl.find voc.vars symb

    let set voc symb value =
        let var = Var.create symb value in
        Hashtbl.replace voc.vars symb var

end (* module Vocabulary *)


module Norm = struct
    
    type t = float -> float -> float

    let minimum x y = if x < y then x else y

    let product x y = x *. y

end (* module Norm *)


module Implication = struct

    type t = float -> FuzzySet.t -> FuzzySet.t

    let larsen activation set =
        FuzzySet.product set activation

end (* module Implication *)


module IsAlso = struct

    type t = FuzzySet.t -> FuzzySet.t -> FuzzySet.t

    let maximum set1 set2 =
        FuzzySet.combine_max set1 set2

end (* module IsAlso *)


module Defuzzyfication = struct

    type t = FuzzySet.t -> float

    let barycenter set =
        FuzzySet.x_cog set

end (* module Defuzzyfication *)


module Premisse = struct

    (* TODO Premisse.Input and Conclusion.Output could use inheritance ...*)

    module Input = struct
        (* fuzzyfier *)

        type t = {
            name: string;
            var:  Var.t;
        }

        let create name var = {
            name = name;
            var = var;
        }

        let fuzzyfy input value =
            FuzzySet.mu (Var.value input.var) value
            
        let name input = input.name

        let to_s input = 
            Printf.sprintf "%s IS %s" input.name (Var.name input.var)

    end (* module Input *)


    type t =
        | Input of Input.t
        | ConnectiveAnd of t * t

    let create_input name var =
        Input (Input.create name var)

    let connect_and prem1 prem2 =
        ConnectiveAnd (prem1, prem2)

    let rec eval premisse norm conorm ctx = match premisse with
        | Input input ->
            Input.fuzzyfy input (FuzIO.get_input ctx (Input.name input))
        | ConnectiveAnd (prem1, prem2) ->
            let act1 = eval prem1 norm conorm ctx in
            let act2 = eval prem2 norm conorm ctx in
            norm act1 act2

    let rec to_s prem = match prem with
        | Input input -> Input.to_s input
        | ConnectiveAnd (prem1, prem2) -> Printf.sprintf "((%s) AND (%s))" (to_s prem1) (to_s prem2)

end (* module Premisse *)


module Conclusion = struct

    module Output = struct

        type t = {
            name: string;
            var:  Var.t;
        }

        let create name var = {
            name = name;
            var = var;
        }

        let eval output activation implication isAlso fuzvalue =
            let res = implication activation (Var.value output.var) in
            match fuzvalue with
            | Some value -> Some (isAlso value res)
            | None -> Some res
            
        let to_s output = 
            Printf.sprintf "%s IS %s" output.name (Var.name output.var)

    end (* module Output *)


    type t =
        | Output of Output.t

    let create_output name var =
        Output (Output.create name var)

    let eval concl activation implication isAlso ctx =
        match concl with
        | Output output ->
            FuzIO.apply_output
                ctx
                output.Output.name
                (Output.eval output activation implication isAlso)

    let to_s concl = match concl with
        | Output output -> Output.to_s output

end (* module Conclusion *)


type operators = {
    opAnd:    Norm.t;
    opImply:  Implication.t;
    opIsAlso: IsAlso.t;
    opDefuzz: Defuzzyfication.t;
}


module Rule = struct
    
    type t = {
        cond:  Premisse.t;
        concl: Conclusion.t;
    }

    let create cond concl = {
        cond = cond;
        concl = concl;
    }

    let to_s rule =
        Printf.sprintf
            "IF %s THEN %s END"
            (Premisse.to_s rule.cond)
            (Conclusion.to_s rule.concl)

    let _eval_activation rule ops ctx =
        Premisse.eval rule.cond ops.opAnd None ctx

    let _eval_conclusion rule act ops ctx =
        Conclusion.eval
            rule.concl
            act
            ops.opImply
            ops.opIsAlso
            ctx

    let eval rule ops ctx =
        let act = _eval_activation rule ops ctx in
        let _ = _eval_conclusion rule act ops ctx in
        act

end (* module Rule *)


(* a model or controller is:
    - a set of rules defining the logic
    - a set of functions defining the fuzzy evaluation
*)
module Controller = struct

    type t = {
        operators: operators;
        rules: Rule.t list;
    }

    let add_rule ctrl rule =
        {ctrl with rules = ctrl.rules @ [ rule; ]}

    let create
        ?(norm=Norm.minimum) (* "a AND b" in rule premisse *)
        ?(implication=Implication.larsen)
        ?(opIsAlso=IsAlso.maximum) (* "output IS V1 AND output IS V2" in rule conclusion *)
        ?(defuzz=Defuzzyfication.barycenter) (* from fuzzy value to crisp one *)
        rules
    =
        let ops = {
            opAnd = norm;
            opImply = implication;
            opIsAlso = opIsAlso;
            opDefuzz = defuzz
        } in
        let ctrl0 = {
            operators = ops;
            rules = [];
        } in
        List.fold_left
            (fun ctrl rule -> add_rule ctrl rule)
            ctrl0
            rules

    let eval ctrl ctx =
        List.fold_left
            (fun idx rule ->
                let act = Rule.eval rule ctrl.operators ctx in
                Printf.printf "Rule %d [%f]: %s\n"
                    idx
                    act
                    (Rule.to_s rule)
                ;
                idx + 1
            )
            0
            ctrl.rules
        
    let defuzz ctrl ctx =
        FuzIO.fold_outputs
            ctx
            (fun name value acc -> (match value with
                | None -> acc
                | Some set -> (name, ctrl.operators.opDefuzz set) :: acc
            ))
            []

end (* module Controller *)


