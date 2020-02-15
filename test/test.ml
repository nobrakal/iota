let functions = ["f";"g"]
let maxprof = 2
let static = ["E"]
let dynamic = ["Active"; "Open"]

let prefix = "tests/"

let good = "good"

let compile x =
  let chan = open_in x in
  let res = Iota.Main.main ~maxprof ~functions ~static ~dynamic chan in
  close_in chan;
  res

let compile_good x =
  match compile x with
  | Ok _ -> true
  | Error _ -> false

let check_compile_good x () = Alcotest.(check bool) x true (compile_good x)

let check_compile_goods dgood =
  let open Alcotest in
  List.map (fun x -> test_case (x ^ " " ^ good) `Quick (check_compile_good (dgood ^ x)))
    (Array.to_list (Sys.readdir dgood))

let parsing () =
  let dir = prefix ^ "parsing/" in
  let dgood = dir ^ "good/" in
  check_compile_goods dgood

let typecheck () =
  let dir = prefix ^ "typechecking/" in
  let dgood = dir ^ "good/" in
  check_compile_goods dgood

let () =
  let open Alcotest in
  run "Iota"
    [ "parsing", parsing ()
    ; "typecheking", typecheck ()]
