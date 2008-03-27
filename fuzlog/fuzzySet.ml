type t = (float * float) list
type choice = Any | First | Second


exception InvalidPossibilityValue of float
exception Divergent


let _check_possibility y =
    if y < 0.0 then raise (InvalidPossibilityValue y);
    if y > 1.0 then raise (InvalidPossibilityValue y);
    ()

let set_point set x y =
    _check_possibility y;
    (* insert in sorted list; all the 'x' are unique *)
    let rec insert res points = match points with
        | (x_, y_) :: q when x_ < x -> insert (res @ [ (x_, y_) ]) q
        | (x_, y_) :: q when x_ = x -> res @ [ (x_, y) ] @ q
        | q -> res @ [ (x, y) ] @ q
    in
    insert [] set

let _create () = []

let create l =
    List.fold_left
        (fun set (x, y) -> set_point set x y)
        (_create ())
        (List.sort
            (fun (xa, _) (xb, _) -> int_of_float (xb -. xa))
            l
        )

let create_triangle x1 x2 =
    (* skew triangle *)
    let s1 = _create () in
    let s2 = set_point s1 x1 0.0 in
    let s3 = set_point s2 (x1 +. (x2 -. x1) /. 2.0) 1.0 in
    let s4 = set_point s3 x2 0.0 in
    s4

let points set = set

let to_s set =
    (List.fold_left
        (fun acc (x, y) -> Printf.sprintf "%s (%f, %f)" acc x y)
        "["
        set
    ) ^ "]"

(* membership function *)
let mu set x = match set with
    (* default is 0.0 *)
    | [] -> 0.0
    (* single point: CRISP value *)
    | (x_, y_) :: [] when x = x_ -> y_
    | (x_, y_) :: [] -> 0.0
    (* more than one point: FUZZY value *)
    | (x_, y) :: _ when x < x_ -> y
    | p0 :: q -> 
        let rec _mu cur points = match points with
            | (x2, y2) :: _ when x < x2 ->
                let x1, y1 = cur in
                y1 +. ((y2 -. y1) /. (x2 -. x1)) *. (x -. x1)
            | p2 :: rest -> _mu p2 rest
            | [] ->
                let x1, y1 = cur in
                y1
        in
        _mu p0 q

let product set a =
    List.map
        (fun (x, y) ->
            let y0 = y *. a in
            let y1 = if y0 > 1.0 then 1.0 else y0 in
            _check_possibility y1;
            (x, y1)
        )
        set

let _merge set1 set2 =
    let rec _rec_merge res s1 s2 = match (s1, s2) with
        | ([], q2) ->
            let tl = List.map (fun (x, y2) -> (x, mu set1 x, y2)) q2 in
            List.rev_append res tl
        | (q1, []) ->
            let tl = List.map (fun (x, y1) -> (x, y1, mu set2 x)) q1 in
            List.rev_append res tl
        | ((x1, y1) :: q1, (x2, y2) :: q2) when x1 < x2 ->
            let m2 = mu set2 x1 in
            _rec_merge ((x1, y1, m2) :: res) q1 ((x2, y2) :: q2)
        | ((x1, y1) :: q1, (x2, y2) :: q2) when x1 > x2 ->
            let m1 = mu set1 x2 in
            _rec_merge ((x2, m1, y2) :: res) ((x1, y1) :: q1) q2
        | ((x1, y1) :: q1, (x2, y2) :: q2) -> (* x1 = x2 *)
            _rec_merge ((x1, y1, y2) :: res) q1 q2
    in
    _rec_merge [] set1 set2

let combine_max set1 set2 =
    let points = _merge set1 set2 in
    let rec _max res cur xyy =
        match xyy with
            | [] ->
                List.rev_map (fun (x, y1, y2, ym) -> (x, ym)) res
            | (x, y1, y2) :: q when y1 = y2 -> 
                _max ((x, y1, y2, y1) :: res) Any q
            | (x, y1, y2) :: q -> 
                let next = if y1 < y2 then Second else First in
                let ym = if y1 < y2 then y2 else y1 in
                let tmpRes =
                    if ((next != cur) && (cur != Any))
                    then
                        let px, py1, py2, pym = List.hd res in
                        let deltaX = x -. px in
                        let deltaY1 = y1 -. py1 in
                        let deltaY2 = y2 -. py2 in
                        let xi = (py1 -. py2) *. deltaX /. (deltaY2 -. deltaY1) in
                        let yi = py1 +. xi *. deltaY1 /. deltaX in
                        let cross = (px +. xi, yi, yi, yi) in
                        cross :: res
                    else
                        res
                in
                _max ((x, y1, y2, ym) :: tmpRes) next q
    in
    let res = _max [] Any points in
    res

let x_cog set =
    let pt0, rest = match set with
        | [] -> raise Divergent
        | p :: [] -> raise Divergent
        | p :: q -> (p, q)
    in
    let (weight, xweight, _) =
        (* compute integral and x*integral by sum of elements *)
        List.fold_left
            (fun (w0, xw0, (x0, y0)) (x, y) ->
                (* rectangle part *)
                let deltaX = x -. x0 in
                let w1 = deltaX *. (min y y0) in
                let xw1 = (x0 +. deltaX /. 2.0) *. w1 in
                (* triangle part *)
                let deltaY = y -. y0 in
                let w2, xtr =
                    if deltaY > 0.0
                    then
                        (0.5 *. deltaX *. deltaY, x0 +. deltaX *. 2.0 /. 3.0)
                    else
                        (-0.5 *. deltaX *. deltaY, x0 +. deltaX /. 3.0)
                in
                let xw2 = xtr *. w2 in
                (w0 +. w1 +. w2, xw0 +. xw1 +. xw2, (x, y))
            )
            (0.0, 0.0, pt0) (* running integ, x*integ, and first point of element *)
            rest
    in
    xweight /. weight


