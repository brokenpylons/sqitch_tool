module Planner = struct
  type t =
    {
      name: string;
      email: string;
    }

  let pp ppf t =
    Fmt.pf ppf "%s<%s>" t.name t.email
end

module Timestamp = struct
  type t =
    {
      year: int;
      month: int;
      day: int;
      hour: int;
      minute: int;
      second: int;
    }

  let pp ppf t =
    Fmt.pf ppf "%04d-%02d-%02dT%02d:%02d:%02dZ" t.year t.month t.day t.hour t.minute t.second
end

module Reference = struct
  type t =
    {
      project: string option;
      change: string option;
      tag: string option;
    }

  let pp ppf t =
    Fmt.pf ppf "%a%a%a"
      Fmt.(option (string ++ const string ":")) t.project
      Fmt.(option string) t.change
      Fmt.(option (const string "@" ++ string)) t.tag
end

module Dependency = struct
  type t = {conflicts: bool; reference: Reference.t}

  let pp ppf t =
    Fmt.pf ppf "%s%a" (if t.conflicts then  "!" else "") Reference.pp t.reference
end

module DependencyList = struct
  type t = Dependency.t list

  let pp ppf = function
    | [] -> Fmt.nop ppf ()
    | l -> Fmt.pf ppf "[%a] " (Fmt.list ~sep:Fmt.(const string " ") Dependency.pp) l
end

module Note = struct
  type t = string option

  let pp ppf = function
    | Some t ->
      Fmt.pf ppf "#%s" t
    | None ->
      Fmt.nop ppf ()
end

module Line = struct
  type blank = {note: Note.t}
  type pragma = {name: string; value: string option}
  type change = {name: string; dependencies: DependencyList.t; timestamp: Timestamp.t; planner: Planner.t; note: Note.t}
  type tag = {name: string; timestamp: Timestamp.t; planner: Planner.t; note: Note.t}
  type t =
    | Blank of blank
    | Pragma of pragma
    | Change of change
    | Tag of tag

  let pp ppf = function
    | Blank x ->
      Fmt.pf ppf "@[%a@]" Note.pp x.note
    | Pragma x ->
      Fmt.pf ppf "@[%%%s%a@]" x.name Fmt.(option (const string "=" ++ string)) x.value
    | Change x ->
      Fmt.pf ppf "@[%s %a%a %a %a@]" x.name DependencyList.pp x.dependencies Timestamp.pp x.timestamp Planner.pp x.planner Note.pp x.note
    | Tag x ->
      Fmt.pf ppf "@[@%s %a %a %a@]" x.name Timestamp.pp x.timestamp Planner.pp x.planner Note.pp x.note
end

type t = Line.t list

let pp ppf x =
  Fmt.pf ppf "@[<v>%a@]" (Fmt.list Line.pp) x

let ( let* ) = Option.bind

let find_change f plan =
  let* line = List.find_opt (function
      | Line.Change x ->
        f x
      | _ -> false)
      plan
  in
  match line with
  | Line.Change x -> Some x
  | _ -> None

class visitor = object(self)

  method plan plan =
    List.filter_map (self#line) plan

  method line = function
    | Line.Blank x ->
      let* note = self#note x.note in
      let* x = self#blank Line.{note} in
      Some (Line.Blank x)

    | Line.Pragma x ->
      let* x = self#pragma x in
      Some (Line.Pragma x)

    | Line.Change x ->
      let dependencies = self#dependencies x.dependencies in
      let* timestamp = self#timestamp x.timestamp in
      let* planner = self#planner x.planner in
      let* note = self#note x.note in
      let* x = self#change Line.{
          name = x.name;
          dependencies;
          timestamp;
          planner;
          note;
        }
      in
      Some (Line.Change x)

    | Line.Tag x ->
      let* timestamp = self#timestamp x.timestamp in
      let* planner = self#planner x.planner in
      let* note = self#note x.note in
      let* x = self#tag Line.{
          name = x.name;
          timestamp;
          planner;
          note;
        }
      in
      Some (Line.Tag x)

  method blank x =
    Some x

  method pragma x =
    Some x

  method change x =
    Some x

  method tag x =
    Some x

  method dependencies x =
    List.filter_map (self#dependency) x

  method dependency x =
    let* reference = self#reference x.reference in
    Some {x with reference}

  method reference x =
    Some x

  method timestamp x =
    Some x

  method planner x =
    Some x

  method note x =
    Some x
end

type lookahead = Code of int | Eof | Empty
type reference_part = Tag of string | Name of string | NameTag of string * string

exception Parse_error

exception Unexpected_eof
exception Unexpected_empty
exception Assert_code of int
exception Name_punctuation
exception Duplicate
exception Save

let unexpected = function
  | Eof -> raise Unexpected_eof
  | Empty -> raise Unexpected_empty
  | Code _ -> raise Parse_error

let punct c =
  0x21 (*!*) <= c && c <= 0x2f (*/*) ||
  0x3a (*:*) <= c && c <= 0x40 (*@*) ||
  0x5b (*[*) <= c && c <= 0x5e (*^*) ||
  c = 0x60 (*`*) ||
  0x7b (*{*) <= c && c <= 0x7e (*~*)

let digit c =
  0x30 (*0*) <= c && c <= 0x39 (*9*)

class parser source = object(self)
  val mutable la: lookahead = Empty
  val decoder: Uutf.decoder = Uutf.decoder source
  val buffer: Buffer.t = Buffer.create 16

  initializer
    self#ignore

  method private plan' lines =
    match la with
    | Code _ ->
      self#blanks;
      let line = self#line in
      self#plan' (line :: lines)
    | Eof ->
      fst @@ List.fold_left (fun (rev, seen) line ->
          let seen = match line with
            | Line.Change x ->
              if List.exists (String.equal x.name) seen then
                raise Duplicate
              else x.name :: seen
            | _ -> seen
          in
          (line :: rev, seen))
        ([], []) lines
    | la -> unexpected la

  method plan =
    self#plan' []

  method line =
    match la with
    | Code 0x25 (*%*) ->
      self#pragma
    | Code 0x40 (*@*) ->
      self#tag
    | Code 0x23 (*#*) | Code 0x0a (*\n*) ->
      self#blank
    | Code _ ->
      self#change
    | la -> unexpected la

  method change =
    let name = self#name in
    self#blanks;
    let dependencies = self#dependencies in
    self#blanks;
    let timestamp = self#timestamp in
    self#blanks;
    let planner = self#planner in
    self#blanks;
    let note = self#note in
    Line.Change {name; dependencies; timestamp; planner; note}

  method pragma =
    self#percent;
    self#blanks;
    let name = self#pragma_name in
    self#blanks;
    let value = self#pragma_value in
    Line.Pragma {name; value}

  method pragma_value =
    match la with
    | Code 0x3d (*=*) ->
      self#eq;
      self#blanks;
      Some self#trailing_text
    | Code 0x0a (*\n*) ->
      self#ignore;
      None
    | la -> unexpected la

  method tag =
    self#at;
    self#blanks;
    let name = self#name in
    self#blanks;
    let timestamp = self#timestamp in
    self#blanks;
    let planner = self#planner in
    self#blanks;
    let note = self#note in
    Line.Tag {name; timestamp; planner; note}

  method blank =
    let note = self#note in
    Line.Blank {note}

  method dependencies =
    match la with
    | Code 0x5b (*[*) ->
      self#lbrac;
      let dependencies = self#dependencies_inner [] in
      self#rbrac;
      dependencies
    | Code _ ->
      []
    | la -> unexpected la

  method dependencies_inner dependencies =
    match la with
    | Code 0x5d (*]*) ->
      List.rev dependencies
    | Code _ ->
      let dependency = self#dependency in
      self#blanks;
      self#dependencies_inner (dependency :: dependencies);
    | la -> unexpected la

  method dependency =
    let conflicts = self#dependency_conflicts in
    let reference = self#dependency_reference in
    {conflicts; reference}

  method dependency_conflicts =
    match la with
    | Code 0x21 (*!*) ->
      self#bang;
      true
    | Code _ ->
      false
    | la -> unexpected la

  method reference_part =
    match la with
    | Code 0x40 (*@*) ->
      self#at;
      let tag = self#name in
      Tag tag
    | Code _ ->
      let name = self#name in
      (match la with
       | Code 0x40 (*@*) ->
         self#at;
         let tag = self#name in
         NameTag (name, tag)
       | Code _ ->
         Name (name)
       | la -> unexpected la)
    | la -> unexpected la

  method dependency_reference =
    match self#reference_part with
    | Tag tag -> Reference.{project = None; change = None; tag = Some tag}
    | NameTag (change, tag) -> Reference.{project = None; change = Some change; tag = Some tag}
    | Name name ->
      (match la with
       | Code 0x3a (*:*) ->
         self#colon;
         (match self#reference_part with
          | Tag tag -> Reference.{project = Some name; change = None; tag = Some tag}
          | NameTag (change, tag) -> Reference.{project = Some name; change = Some change; tag = Some tag}
          | Name change -> Reference.{project = Some name; change = Some change; tag = None})
       | Code _ ->
         Reference.{project = None; change = Some name; tag = None}
       | la -> unexpected la)

  method timestamp =
    let year = self#year in
    self#dash;
    let month = self#month in
    self#dash;
    let day = self#day in
    self#time;
    let hour = self#hour in
    self#colon;
    let minute = self#hour in
    self#colon;
    let second = self#hour in
    self#zulu;
    {year; month; day; hour; minute; second}

  method year =
    self#digit;
    self#digit;
    self#digit;
    self#digit;
    int_of_string self#get_buffer

  method month =
    self#digit;
    self#digit;
    int_of_string self#get_buffer

  method day =
    self#digit;
    self#digit;
    int_of_string self#get_buffer

  method hour =
    self#digit;
    self#digit;
    int_of_string self#get_buffer

  method minute =
    self#digit;
    self#digit;
    int_of_string self#get_buffer

  method second =
    self#digit;
    self#digit;
    int_of_string self#get_buffer

  method planner =
    let name = self#planner_name in
    self#langle;
    let email = self#planner_email in
    self#rangle;
    {name; email}

  method planner_name =
    match la with
    | Code 0x3c (*<*) ->
      self#get_buffer
    | Code _ ->
      self#read;
      self#planner_name
    | la -> unexpected la

  method planner_email =
    match la with
    | Code 0x3e (*>*) ->
      self#get_buffer
    | Code _ ->
      self#read;
      self#planner_email
    | la -> unexpected la

  method pragma_name =
    match la with
    | Code c when punct c ->
      raise Name_punctuation
    | Code _ ->
      self#read;
      self#pragma_name_rest
    | la -> unexpected la

  method pragma_name_rest =
    match la with
    | Code 0x20 (* *) | Code 0x9 (*\t*) | Code 0x0a (*\n*) | Code 0x3d (*=*) ->
      self#get_buffer
    | Code c when punct c ->
      self#read;
      self#name_punct
    | Code _ ->
      self#read;
      self#pragma_name_rest
    | la -> unexpected la

  method pragma_name_punct =
    match la with
    | Code 0x20 (* *) | Code 0x9 (*\t*) | Code 0x0a (*\n*) | Code 0x3d (*=*) ->
      raise Name_punctuation
    | Code _ ->
      self#read;
      self#name_rest
    | la -> unexpected la

  method name =
    match la with
    | Code c when punct c ->
      raise Name_punctuation
    | Code _ ->
      self#read;
      self#name_rest
    | la -> unexpected la

  method name_rest =
    match la with
    | Code 0x20 (* *) | Code 0x9 (*\t*) | Code 0x0a (*\n*)
    | Code 0x3a (*:*) | Code 0x40 (*@*) | Code 0x23 (*#*) | Code 0x5c (*\*) | Code 0x5d (*]*)  ->
      self#get_buffer
    | Code 0x7e (*~*) | Code 0x2f (*/*) | Code 0x3d (*=*) | Code 0x25 (*%*) | Code 0x5e (*^*) ->
      self#read;
      self#name_digits
    | Code c when punct c ->
      self#read;
      self#name_punct
    | Code _ ->
      self#read;
      self#name_rest
    | la -> unexpected la

  method name_punct =
    match la with
    | Code 0x20 (* *) | Code 0x9 (*\t*) | Code 0x0a (*\n*)
    | Code 0x3a (*:*) | Code 0x40 (*@*) | Code 0x23 (*#*) | Code 0x5c (*\*) | Code 0x5d (*]*)  ->
      raise Name_punctuation
    | Code _ ->
      self#read;
      self#name_rest
    | la -> unexpected la

  method name_digits =
    match la with
    | Code c when digit c ->
      self#read;
      self#name_digits
    | Code 0x20 (* *) | Code 0x9 (*\t*) | Code 0x0a (*\n*)
    | Code 0x3a (*:*) | Code 0x40 (*@*) | Code 0x23 (*#*) | Code 0x5c (*\*) | Code 0x5d (*]*)  ->
      raise Name_punctuation
    | Code _ ->
      self#read;
      self#name_rest
    | la -> unexpected la

  method note =
    match la with
    | Code 0x23 (*#*) ->
      self#hash;
      Some self#trailing_text
    | Code 0x0a (*\n*) ->
      self#ignore;
      None
    | la -> unexpected la

  method trailing_text =
    match la with
    | Code 0x0a (*\n*) ->
      self#ignore;
      self#get_buffer
    | Code _ ->
      self#read;
      self#trailing_text
    | la -> unexpected la

  method blanks =
    match la with
    | Code 0x20 (* *) | Code 0x9 (*\t*) ->
      self#ignore;
      self#blanks
    | Code _ ->
      ()
    | la -> unexpected la

  method digit =
    match la with
    | Code c when digit c ->
      self#read
    | _ -> raise Parse_error

  method lbrac =
    self#assert_code 0x5b (*[*)

  method rbrac =
    self#assert_code 0x5d (*]*)

  method langle =
    self#assert_code 0x3c (*<*)

  method rangle =
    self#assert_code 0x3e (*>*)

  method eq =
    self#assert_code 0x3d (*=*)

  method at =
    self#assert_code 0x40 (*@*)

  method percent =
    self#assert_code 0x25 (*%*)

  method hash =
    self#assert_code 0x23 (*#*)

  method bang =
    self#assert_code 0x3a (*!*)

  method dash =
    self#assert_code 0x2d (*-*)

  method colon =
    self#assert_code 0x3a (*:*)

  method time =
    self#assert_code 0x54 (*T*)

  method zulu =
    self#assert_code 0x5a (*Z*)

  method assert_code d =
    match la with
    | Code c when c == d ->
      self#ignore
    | _ -> raise (Assert_code d)

  method private read =
    self#get_code true

  method private ignore =
    self#get_code false

  method private get_buffer: string =
    let s = Buffer.contents buffer in
    Buffer.clear buffer;
    s

  method private get_code save =
    if save then
      (match la with
       | Code c -> Buffer.add_utf_8_uchar buffer (Uchar.of_int c)
       | _ -> raise Save);
    match Uutf.decode decoder with
    | `Uchar c ->
      la <- Code (Uchar.to_int c)
    | `End ->
      la <- Eof
    | _ -> raise Parse_error
end
