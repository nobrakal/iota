open Iota

let main verbose dinfer_guards configfile filename =
  let infer_guards = not dinfer_guards in
  let options = Main.{verbose;infer_guards} in
  let configchan = Lexing.from_channel (open_in configfile) in
  let chan = Lexing.from_channel (open_in filename) in
  match Main.main options (Main.config configchan) chan with
  | Error s ->
     Main.print_err s
  | Ok ast ->
     Final_def.print_final ast

open Cmdliner

let config =
  let doc = "The Iota configuration file." in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"HIOTA_FILE")

let file =
  let doc = "The Iota file." in
  Arg.(required & pos 1 (some string) None & info [] ~doc ~docv:"IOTA_FILE")

let verbose =
  let doc = "Be more verbose." in
  Arg.(value & flag & info ["v";"verbose"] ~doc)

let dinfer_guards =
  let doc = "Disable guard inference." in
  Arg.(value & flag & info ["d";"dinfer"] ~doc)

let info =
  let doc = "Run the Iota compiler." in
  let man =
    [ `S Manpage.s_bugs
    ; `P "Report bugs to: https://github.com/nobrakal/iota/issues."
    ; `S Manpage.s_see_also
    ; `P "Project homepage: https://github.com/nobrakal/iota/" ]
  in
  Term.info "iota" ~doc ~exits:Term.default_exits ~man

let main_t = Term.(const main $verbose $ dinfer_guards $ config $ file)

let () = Term.exit @@ Term.eval (main_t, info)
