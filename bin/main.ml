open Iota

let main configfile filename =
  let configchan = Lexing.from_channel (open_in configfile) in
  let chan = Lexing.from_channel (open_in filename) in
  match Main.main (Main.config configchan) chan with
  | Error s ->
     Main.print_err s
  | Ok ast ->
     Final.print_final ast

let () = main Sys.argv.(1) Sys.argv.(2)
