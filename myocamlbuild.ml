open Ocamlbuild_plugin
open Command
(* http://brion.inria.fr/gallium/index.php/Using_ocamlfind_with_ocamlbuild *)
(*
module PN = Pathname

let split path =
  let rec aux path =
    if path = Filename.current_dir_name then []
    else (Filename.basename path) :: aux (Filename.dirname path)
  in List.rev (aux path)

let ends_with ew s =
  let sl = String.length s in
  let ewl = String.length ew in
  sl >= ewl && String.sub s (sl - ewl) ewl = ew

let starts_with sw s =
  let sl = String.length s in
  let swl = String.length sw in
  sl >= swl && String.sub s 0 swl = sw

let rec find pred sofar fn =
  if starts_with "_build" fn || ends_with ".svn" fn
  then sofar
  else begin
    if PN.is_directory fn
    then
      let fns = PN.readdir fn in
      let fns = Array.map (PN.concat fn) fns in
      Array.fold_left (find pred) sofar fns
    else if pred fn then fn::sofar else sofar
  end

let make_suite = A"make_suite.byte"

let test_files () =
    let is_test_fn fn =
        starts_with "test_" (Filename.basename fn) ||
            List.mem "test" (split (Filename.dirname fn))
    in
    find is_test_fn [] "."

let make_tests files =
  rule "make tests"
    ~prod:"tests.ml"
    ~deps:files
    begin fun _ _ ->
        Cmd (S ([make_suite; A"-o"; Px"tests.ml"] @
               (List.map (fun s -> P s) files)));
    end
*)


(* these functions are not really officially exported *)
let run_and_read = Ocamlbuild_pack.My_unix.run_and_read
let blank_sep_strings = Ocamlbuild_pack.Lexers.blank_sep_strings
   
(* this lists all supported packages *)
let find_packages () =
    blank_sep_strings &
        Lexing.from_string &
        run_and_read "ocamlfind list | cut -d' ' -f1"
                     
(* this is supposed to list available syntaxes, but I don't know how to do it. *)
let find_syntaxes () = ["camlp4o"; "camlp4r"]
                        
(* ocamlfind command *)
let ocamlfind x = S[A"ocamlfind"; x]

let _ = dispatch begin function
    | Before_options ->
        (* by using Before_options one let command line options have an higher priority *)
        (* on the contrary using After_options will guarantee to have the higher priority *)

        (* override default commands by ocamlfind ones *)
        Options.ocamlc   := ocamlfind & A"ocamlc";
        Options.ocamlopt := ocamlfind & A"ocamlopt";
        Options.ocamldep := ocamlfind & A"ocamldep";
        Options.ocamldoc := ocamlfind & A"ocamldoc"
    | After_rules ->
        (*make_tests (test_files ());*)
        
        (* When one link an OCaml library/binary/package, one should use -linkpkg *)
        flag ["ocaml"; "link"] & A"-linkpkg";
                
        (* For each ocamlfind package one inject the -package option when
           * compiling, computing dependencies, generating documentation and
           * linking. *)
        List.iter begin fun pkg ->
            flag ["ocaml"; "compile";  "pkg_"^pkg] & S[A"-package"; A pkg];
            flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-package"; A pkg];
            flag ["ocaml"; "doc";      "pkg_"^pkg] & S[A"-package"; A pkg];
            flag ["ocaml"; "link";     "pkg_"^pkg] & S[A"-package"; A pkg];
        end (find_packages ());
                                                                                                         
        (* Like -package but for extensions syntax. Morover -syntax is useless
        * when linking. *)
        List.iter begin fun syntax ->
            flag ["ocaml"; "compile";  "syntax_"^syntax] & S[A"-syntax"; A syntax];
            flag ["ocaml"; "ocamldep"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
            flag ["ocaml"; "doc";      "syntax_"^syntax] & S[A"-syntax"; A syntax];
        end (find_syntaxes ());
        
        (* The default "thread" tag is not compatible with ocamlfind.
        Indeed, the default rules add the "threads.cma" or "threads.cmxa"
        options when using this tag. When using the "-linkpkg" option with
        ocamlfind, this module will then be added twice on the command line.
        
        To solve this, one approach is to add the "-thread" option when using
        the "threads" package using the previous plugin.
        *)
        flag ["ocaml"; "pkg_threads"; "compile"] (S[A "-thread"]);
        flag ["ocaml"; "pkg_threads"; "link"] (S[A "-thread"])
    | _ -> ()
end
