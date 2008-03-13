let _cmp_float a b =
    let d = a -. b in
    abs_float d < 0.01

let _cmp_points l1 l2 =
    List.fold_left2
        (fun acc (xa, ya) (xb, yb) ->
            let r1 = _cmp_float xa xb in
            let r2 = _cmp_float ya yb in
            acc && r1 && r2
        )
        true
        l1
        l2

let _mu_check s x expected =
    let y = FuzzySet.mu s x in
    (*let _ = Printf.printf "mu(%f) = %f\n" x y in*)
    OUnit.assert_equal
        ~cmp:_cmp_float
        ~msg:"membership function"
        ~printer:(fun v -> Printf.sprintf "mu(%f)=%f" x v)
        expected
        y

let test_create () =
    let s = FuzzySet.create [
        (2.0, 0.8);
        (3.0, 0.0);
        (1.0, 0.0);
        (2.0, 1.0);
    ] in
    OUnit.assert_equal
        ~cmp:_cmp_points
        ~msg:"create from point list"
        [(1.0, 0.0); (2.0, 1.0); (3.0, 0.0);]
        (FuzzySet.points s)
    ;
    OUnit.assert_raises
        (FuzzySet.InvalidPossibilityValue (-0.1))
        (fun () -> FuzzySet.create [ (2.0, -0.1); ])
    ;
    OUnit.assert_raises
        (FuzzySet.InvalidPossibilityValue (1.1))
        (fun () -> FuzzySet.create [ (2.0, 1.1); ])

let test_create_triangle () =
    let s = FuzzySet.create_triangle 1.0 3.0 in
    OUnit.assert_equal
        ~cmp:_cmp_points
        ~msg:"create skew triangle"
        [(1.0, 0.0); (2.0, 1.0); (3.0, 0.0);]
        (FuzzySet.points s)
    
let test_to_s () =
    let s = FuzzySet.create_triangle 12.0 42.0 in
    let _ = FuzzySet.to_s s in
    OUnit.assert_bool "to_s failure" true

let test_mu () =
    let s = FuzzySet.create_triangle 1.0 3.0 in
    let _ = _mu_check s 0.5 0.0 in
    let _ = _mu_check s 1.0 0.0 in
    let _ = _mu_check s 1.3 0.3 in
    let _ = _mu_check s 1.5 0.5 in
    let _ = _mu_check s 1.6 0.6 in
    let _ = _mu_check s 2.0 1.0 in
    let _ = _mu_check s 2.5 0.5 in
    let _ = _mu_check s 3.0 0.0 in
    let _ = _mu_check s 3.5 0.0 in
    ()

let test_product () =
    let s0 = FuzzySet.create [
        (-1.0, 0.2);
        (2.0, 1.0);
        (5.5, 0.8);
        (6.5, 0.3);
    ] in
    let s1 = FuzzySet.product s0 2.0 in
    OUnit.assert_equal
        ~cmp:_cmp_points
        ~msg:"product with saturation"
        [(-1.0, 0.4); (2.0, 1.0); (5.5, 1.0); (6.5, 0.6);]
        (FuzzySet.points s1)

let test_combine_max () =
    let s1 = FuzzySet.create_triangle 0.0 4.0 in
    let s2 = FuzzySet.create_triangle 2.0 6.0 in
    let max = FuzzySet.combine_max s1 s2 in
    OUnit.assert_equal
        ~cmp:_cmp_points
        ~msg:"combine maximum"
        [(0.0, 0.0); (2.0, 1.0); (3.0, 0.5); (4.0, 1.0); (6.0, 0.0);]
        (FuzzySet.points max)

let test_x_cdg () =
    let s1 = FuzzySet.create_triangle 0.0 4.0 in
    let s2 = FuzzySet.create_triangle 2.0 6.0 in
    let comb = FuzzySet.combine_max s1 s2 in
    let tester = fun set expected ->
        let x = FuzzySet.x_cdg set in
        OUnit.assert_equal
            ~cmp:_cmp_float
            ~msg:"x_cdg"
            ~printer:(fun v -> Printf.sprintf "x_cdg=%f" v)
            expected
            x
    in
    tester s1 2.0;
    tester s2 4.0;
    tester comb 3.0

