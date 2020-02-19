let functions = ["f";"g"]
let maxprof = 2
let static = ["E"]
let dynamic = ["Active"; "Open"]

let prefix = "tests/"

let good = "good"

let compile' x = Iota.Main.main ~maxprof ~functions ~static ~dynamic x

let compile x =
  let chan = open_in x in
  let res = compile' (Lexing.from_channel chan) in
  close_in chan;
  res

let compile_good x =
  match compile x with
  | Ok _ -> true
  | Error _ -> false

let tt = Alcotest.testable (Fmt.of_to_string (Iota.Final.string_of_final)) ( = )

let reentrant_compile x () =
  match compile x with
  | Error _ -> Alcotest.fail x
  | Ok y ->
     match compile' (Lexing.from_string (Iota.Final.string_of_final y)) with
     | Error _ -> Alcotest.fail x
     | Ok y' ->
     Alcotest.check tt x
       (Iota.Final.normalize y)
       (Iota.Final.normalize y')

let check_pred p x () = Alcotest.(check bool) x true (p x)

let list_of_files x = Array.to_list (Sys.readdir x)

let check_compile_goods dgood =
  let aux x =
    let open Alcotest in
    test_case (x ^ " " ^ good) `Quick (check_pred compile_good (dgood ^ x))
  in List.map aux (list_of_files dgood)

let check_reentrant_compil dgood =
  let aux x =
    let open Alcotest in
    test_case (x ^ " " ^ good) `Quick (reentrant_compile (dgood ^ x))
  in List.map aux (list_of_files dgood)

let test_good_cases f c =
  let dir = prefix ^ c in
  let dgood = dir ^ "good/" in
  f dgood

let test_dir dir =
  let dir' = dir ^ "/" in
  [ dir, test_good_cases check_compile_goods dir'
  ; dir ^ " reentrant", test_good_cases check_reentrant_compil dir' ]

let () =
  let open Alcotest in
  run "Iota"
    (test_dir "parsing" @ test_dir "typechecking" @ test_dir "structure")
