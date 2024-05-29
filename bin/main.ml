open Cmdliner
open Sqitch_tool

let version = "0.0.0"

let conf =
  let docs = Manpage.s_common_options in
  let plan_file =
    let doc = "Path to the deployment plan file." in
    Arg.(value & opt (some string) None & info ["plan_file"] ~docs ~doc)
  in
  let top_dir =
    let doc = "Top dir." in
    Arg.(value & opt (some string) None & info ["top_dir"] ~docs ~doc)
  in
  Term.(const Actions.conf $ plan_file $ top_dir)

let sdocs = Manpage.s_common_options

let plan conf =
  Actions.plan conf;
  `Ok ()

let move conf source target =
  Actions.move conf source target;
  `Ok ()

let copy conf source target =
  Actions.copy conf source target;
  `Ok ()

let remove conf target =
  Actions.remove conf target;
  `Ok ()

let plan_cmd =
  let doc = "Print plan" in
  let info = Cmd.info "plan" ~version ~doc ~sdocs in
  Cmd.v info Term.(ret (const plan $ conf))

let move_cmd =
  let doc = "Move change" in
  let source =
    let doc = "Source change." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"SOURCE" ~doc)
  in
  let target =
    let doc = "Target change." in
    Arg.(required & pos 1 (some string) None & info [] ~docv:"TARGET" ~doc)
  in
  let info = Cmd.info "mv" ~version ~doc ~sdocs in
  Cmd.v info Term.(ret (const move $ conf $ source $ target))

let copy_cmd =
  let doc = "Copy change" in
  let source =
    let doc = "Source change." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"SOURCE" ~doc)
  in
  let target =
    let doc = "Target change." in
    Arg.(required & pos 1 (some string) None & info [] ~docv:"TARGET" ~doc)
  in
  let info = Cmd.info "cp" ~version ~doc ~sdocs in
  Cmd.v info Term.(ret (const copy $ conf $ source $ target))

let remove_cmd =
  let doc = "Remove change" in
  let target =
    let doc = "Target change." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"TARGET" ~doc)
  in
  let info = Cmd.info "rm" ~version ~doc ~sdocs in
  Cmd.v info Term.(ret (const remove $ conf $ target))

let help_secs = [
 `S Manpage.s_common_options;
 `P "These options are common to all commands.";
 `S "MORE HELP";
 `P "Use $(mname) $(i,COMMAND) --help for help on a single command.";
 `S Manpage.s_bugs; `P "https://github.com/brokenpylons/sqitch_tool/issues";]


let main_cmd =
  let doc = "a sqitch utility" in
  let man = help_secs in
  let info = Cmd.info "sqitch_tool" ~version ~doc ~sdocs ~man in
  let default = Term.(ret (const (fun _ -> `Help (`Pager, None)) $ conf)) in
  Cmd.group info ~default [plan_cmd; move_cmd; copy_cmd; remove_cmd]


let main () = exit (Cmd.eval main_cmd)
let () = main ()
