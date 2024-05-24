

let p = new Sqitch_tool.Plan.parser (Uutf.decoder (`String "
%syntax-version=1.0.0
%project=test

schema 2024-05-23T11:14:35Z Žiga Leber <mail+github@zigaleber.com> # Add schema
@1.0.0 2024-05-23T11:14:46Z Žiga Leber <mail+github@zigaleber.com> # Deploy
schema [schema@1.0.0] 2024-05-23T11:16:00Z Žiga Leber <mail+github@zigaleber.com> # Something
"))

let () =
  let plan = p#plan in
  Fmt.pr "%a" Sqitch_tool.Plan.pp plan;
  ()
