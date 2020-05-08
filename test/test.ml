let prefix = "tests/"

let good = "good"
let bad = "bad"

type ttt = Parsing | Typechecking | Structure | Guard_Inference | Simplification

let string_of_ttt = function
  | Parsing -> "parsing"
  | Typechecking -> "typechecking"
  | Structure -> "structure"
  | Guard_Inference -> "guard_inference"
  | Simplification -> "simplification"

let config = Iota.Main.config (Lexing.from_channel (open_in (prefix ^ "config.hiota")))

let compile' x = Iota.Main.main Iota.Main.default_options config x

let compile x =
  let chan = open_in x in
  let res = compile' (Lexing.from_channel chan) in
  close_in chan;
  res

let compile_good x =
  match compile x with
  | Ok _ -> true
  | Error _ -> false

let compile_bad ttt x =
  match compile x,ttt with
  | Error (Iota.Main.Type _), Typechecking
    | Error (Iota.Main.Parse _ | Iota.Main.Menhir), Parsing
    | Error (Iota.Main.Structure _), Structure
    | Error (Iota.Main.Simplification _), Simplification
    | Error (Iota.Main.GuardInference _), Guard_Inference -> true
  | _ -> false

let tt = Alcotest.testable (Fmt.of_to_string (Iota.Final_def.string_of_final)) ( = )

let reentrant_compile x () =
  match compile x with
  | Error _ -> Alcotest.fail x
  | Ok y ->
     match compile' (Lexing.from_string (Iota.Final_def.string_of_final y)) with
     | Error _ -> Alcotest.fail x
     | Ok y' ->
     Alcotest.check tt x y y'

let check_pred p x () = Alcotest.(check bool) x true (p x)

let list_of_files x = Array.to_list (Sys.readdir x)

let check_dir suffix f dir =
  let aux x =
    let open Alcotest in
    test_case (x ^ " " ^ suffix) `Quick (f (dir ^ x))
  in List.map aux (list_of_files dir)

let test_good_cases f c =
  f (prefix ^ c ^ "good/")

let test_bad_cases f c =
  f (prefix ^ c ^ "bad/")

let test_dir ttt =
  let dir = string_of_ttt ttt in
  let dir' =  dir ^ "/" in
  let check_compile_goods =
    check_dir good (check_pred compile_good) in
  let check_reentrant_compil =
    check_dir good reentrant_compile in
  let check_bad =
    check_dir bad (check_pred (compile_bad ttt)) in
  [ dir, test_good_cases check_compile_goods dir'
  ; dir ^ " reentrant", test_good_cases check_reentrant_compil dir'
  ; dir ^ " bad", test_bad_cases check_bad dir']

let () =
  let open Alcotest in
  run "Iota"
    (test_dir Parsing @ test_dir Typechecking @ test_dir Structure @ test_dir Guard_Inference)
