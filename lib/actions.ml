let (//) = Filename.concat

type conf =
  {
    plan_file: string;
    top_dir: string
  }

let conf plan_file top_dir =
  {
    plan_file = Option.value ~default:"sqitch.plan" plan_file;
    top_dir = Option.value ~default:"." top_dir;
  }

let read_plan conf =
  let f = open_in conf.plan_file in
  let parser = new Plan.parser (`Channel f) in
  let plan = parser#plan in
  close_in f;
  Fmt.pr "%a@." Plan.pp plan

let transform_plan conf visitor =
  let f = open_in conf.plan_file in
  let parser = new Plan.parser (`Channel f) in
  let plan = parser#plan in
  close_in f;
  let plan = visitor#plan plan in
  let f = open_out conf.plan_file in
  Fmt.pf (Format.formatter_of_out_channel f) "%a@." Plan.pp plan;
  flush f;
  close_out f

let plan conf =
  read_plan conf

(* Move *)

class move_visitor source target = object
  inherit Plan.visitor

  method! change x =
    Some (if source = x.name
          then {x with name = target}
          else x)

  method! reference x =
    Some (match x.change with
        | Some change ->
          if source = change
          then {x with change = Some target}
          else x
        | None -> x)
end

let move_files conf source target =
  let open FileUtil in
  let source_file = source ^ ".sql" in
  let target_file = target ^ ".sql" in
  let target_dir = Filename.dirname target in
  mkdir ~parent:true (conf.top_dir // "deploy" // target_dir);
  mkdir ~parent:true (conf.top_dir // "verify" // target_dir);
  mkdir ~parent:true (conf.top_dir // "revert" // target_dir);

  mv (conf.top_dir // "deploy" // source_file) (conf.top_dir // "deploy" // target_file);
  mv (conf.top_dir // "verify" // source_file) (conf.top_dir // "verify" // target_file);
  mv (conf.top_dir // "revert" // source_file) (conf.top_dir // "revert" // target_file)

let move conf source target =
  let visitor = new move_visitor source target in
  move_files conf source target;
  transform_plan conf visitor

(* Remove *)

class remove_visitor target = object
  inherit Plan.visitor

  method! change x =
    if target = x.name
    then None
    else Some x

  method! reference x =
    (match x.change with
     | Some change ->
       if target = change
       then None
       else Some x
     | None -> Some x)
end

let remove_files conf target =
  let open FileUtil in
  let target_file = target ^ ".sql" in
  rm [conf.top_dir // "deploy" // target_file];
  rm [conf.top_dir // "verify" // target_file];
  rm [conf.top_dir // "revert" // target_file]

let remove conf target =
  let visitor = new remove_visitor target in
  remove_files conf target;
  transform_plan conf visitor
