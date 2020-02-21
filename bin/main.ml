open Iota

let main filename =
  let chan = open_in filename in
  let functions = ["f";"g"] in
  let maxprof = 1 in
  let static = ["E"] in
  let dynamic = ["Active"; "Open"] in
  match Main.main ~maxprof ~functions ~static ~dynamic (Lexing.from_channel chan) with
  | Error s ->
     Main.print_err s
  | Ok ast ->
     Final.print_final ast

let () = main (Sys.argv.(1))
