
module Planner = struct
  type t =
    {
      name: string;
      email: string;
    }

  let pp ppf t =
    Fmt.pf ppf "@[%s <%s>@]" t.name t.email
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
    Fmt.pf ppf "@[%04d-%02d-%02dT%02d:%02d:%02dZ@]" t.year t.month t.day t.hour t.minute t.second
end

module Dependency = struct
  type t =
    | Requires of string
    | Conflicts of string

  let pp ppf = function
    | Requires t ->
      Fmt.pf ppf "%s" t
    | Conflicts t ->
      Fmt.pf ppf "!%s" t
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
  type t =
    | Blank of {note: Note.t}
    | Pragma of {name: string; value: string}
    | Change of {name: string; dependencies: Dependency.t list; timestamp: Timestamp.t; planner: Planner.t; note: Note.t}
    | Tag of {name: string; timestamp: Timestamp.t; planner: Planner.t; note: Note.t}

  let pp ppf = function
    | Blank t ->
      Fmt.pf ppf "@[%a@]" Note.pp t.note
    | Pragma t ->
      Fmt.pf ppf "@[%%%s=%s@]" t.name t.value
    | Change t ->
      Fmt.pf ppf "@[%s [@[%a@]] %a %a %a@]" t.name (Fmt.list Dependency.pp) t.dependencies Timestamp.pp t.timestamp Planner.pp t.planner Note.pp t.note
    | Tag t ->
      Fmt.pf ppf "@[@%s %a %a %a@]" t.name Timestamp.pp t.timestamp Planner.pp t.planner Note.pp t.note
end

type t = Line.t list

let pp ppf t =
  Fmt.pf ppf "@[<v>%a@]" (Fmt.list Line.pp) t

type lookahead = Code of int | Eof | Empty

exception Parse_error

exception Unexpected_eof
exception Unexpected_empty
exception Assert_code of int
exception Name_punctuation
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

let special = function
  | 0x3a (*:*) | 0x40 (*@*) | 0x23 (*#*) | 0x5c (*\*) -> true
  | _ -> false

  (*| 0x7e (*~*) | 0x2f (*/*) | 0x3d (*=*) | 0x25 (*%*) | 0x5b (*[*) | 0x5d (*]*) -> true 
  | _ -> false*)

(*let blank = function
  | 0x20 (* *) | 0x9 (*\t*) -> true
  | _ -> false*)

let whitespace = function
  | 0x20 (* *) | 0x9 (*\t*) | 0x0a (*\n*) -> true
  | _ -> false

let digit c =
  0x30 (*0*) <= c && c <= 0x39 (*9*)

class parser decoder = object(self)
  val mutable la0: lookahead = Empty
  val mutable la1: lookahead = Empty
  val decoder: Uutf.decoder = decoder
  val buffer: Buffer.t = Buffer.create 16

  initializer
    self#ignore;
    self#ignore

  method plan =
    match la0 with
    | Code _ ->
      self#blanks;
      let line = self#line in
      line :: self#plan
    | Eof ->
      []
    | la -> unexpected la

  method line =
    match la0 with
    | Code 0x25 (*%*) ->
      self#pragma
    | Code 0x40 (*@*) ->
      self#tag
    | Code 0x23 (*#*) | Code 0x0a (*\n*) ->
      self#note_line
    | Code _ ->
      self#change
    | la -> unexpected la

  method change =
    let name = self#name (fun c -> c = 0x5b (*]*)) in
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
    let name = self#name (fun c -> c = 0x3d (*=*)) in
    self#blanks;
    self#eq;
    self#blanks;
    let value = self#trailing_text in
    Line.Pragma {name; value}

  method tag =
    self#at;
    self#blanks;
    let name = self#name (fun _ -> true) in
    self#blanks;
    let timestamp = self#timestamp in
    self#blanks;
    let planner = self#planner in
    self#blanks;
    let note = self#note in
    Line.Tag {name; timestamp; planner; note}

  method note_line =
    let note = self#note in
    Line.Blank {note}

  method dependencies =
    match la0 with
    | Code 0x5b (*[*) ->
      self#lbrac;
      let dependencies = self#dependencies_inner in
      self#rbrac;
      dependencies
    | Code _ ->
      []
    | la -> unexpected la

  method dependencies_inner =
    match la0 with
    | Code 0x5d (*]*) ->
      []
    | Code 0x21 (*!*) ->
      let conflicts = self#conflicts in
      conflicts :: self#dependencies_inner
    | Code _ ->
      let requires = self#requires in
      requires :: self#dependencies_inner
    | la -> unexpected la

  method requires =
    Requires (self#name (fun c -> c = 0x5b (*]*)))

  method conflicts =
    self#bang;
    Conflicts (self#name (fun c -> c = 0x5b (*]*)))

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
    self#readp digit;
    self#readp digit;
    self#readp digit;
    self#readp digit;
    int_of_string self#get_buffer

  method month =
    self#readp digit;
    self#readp digit;
    int_of_string self#get_buffer

  method day =
    self#readp digit;
    self#readp digit;
    int_of_string self#get_buffer

  method hour =
    self#readp digit;
    self#readp digit;
    int_of_string self#get_buffer

  method minute =
    self#readp digit;
    self#readp digit;
    int_of_string self#get_buffer

  method second =
    self#readp digit;
    self#readp digit;
    int_of_string self#get_buffer

  method planner =
    let name = self#planner_name in
    self#langle;
    let email = self#planner_email in
    self#rangle;
    {name; email}

  method planner_name =
    match la0 with
    | Code 0x3c (*<*) ->
      self#get_buffer
    | Code _ ->
      self#read;
      self#planner_name
    | la -> unexpected la

  method planner_email =
    match la0 with
    | Code 0x3e (*>*) ->
      self#get_buffer
    | Code _ ->
      self#read;
      self#planner_email
    | la -> unexpected la

  method name p =
    match la0 with
    | Code c when punct c ->
      raise Name_punctuation
    | Code _ ->
      self#read;
      self#name_rest p
    | la -> unexpected la

  method name_rest p =
    match la1 with
    | Code c when whitespace c || p c ->
      (match la0 with
       | Code c when punct c ->
         Fmt.pr "%i" c;
         raise Name_punctuation
       | Code _ ->
         self#read;
         self#get_buffer
       | la -> unexpected la)
    | Code c when not (special c) ->
      self#read;
      self#name_rest p
    | la -> unexpected la

  method note =
    match la0 with
    | Code 0x23 (*#*) ->
      self#hash;
      Some self#trailing_text
    | Code 0x0a (*\n*) ->
      self#ignore;
      None
    | la -> unexpected la

  method trailing_text =
    match la0 with
    | Code 0x0a (*\n*) ->
      self#ignore;
      self#get_buffer
    | Code _ ->
      self#read;
      self#trailing_text
    | la -> unexpected la

  method blanks =
    match la0 with
    | Code 0x20 (* *) | Code 0x9 (*\t*) ->
      self#ignore;
      self#blanks
    | Code _ ->
      ()
    | la -> unexpected la

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
    match la0 with
    | Code c when c == d ->
      self#ignore
    | _ -> raise (Assert_code d)

  method readp p =
    match la0 with
    | Code c when p c ->
      self#read
    | _ -> raise Parse_error

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
      (match la0 with
       | Code c -> Buffer.add_utf_8_uchar buffer (Uchar.of_int c)
       | _ -> raise Save);
    la0 <- la1;
    match Uutf.decode decoder with
    | `Uchar c ->
      la1 <- Code (Uchar.to_int c)
    | `End ->
      la1 <- Eof
    | _ -> raise Parse_error
end
