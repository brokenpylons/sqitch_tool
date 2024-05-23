type planner =
  {
    name: string;
    email: string;
  }

type timestamp =
  {
    year: int;
    month: int;
    day: int;
    hour: int;
    minute: int;
    second: int;
  }

type meta =
  {
    planner: planner;
    timestamp: timestamp;
  }

type line =
  | Blank of {note: string option}
  | Pragma of {name: string; value: string}
  | Change of {name: string; requires: string list; conflicts: string list; meta: meta; note: string option}
  | Tag of {name: string; meta: meta; note: string option}

type lookahead = Code of int | Eof

exception ParseError

let punct c =
  0x21 (*!*) <= c && c <= 0x2f (*/*) ||
  0x3a (*:*) <= c && c <= 0x40 (*@*) ||
  0x5b (*[*) <= c && c <= 0x5e (*^*) ||
  c = 0x60 (*`*) ||
  0x7b (*{*) <= c && c <= 0x7e (*~*)

let digit c =
  0x30 <= c && c <= 0x39

class parser = object(self)
  val mutable la0: lookahead
  val mutable la1: lookahead
  val decoder: Uutf.Decoder.t
  val buffer: Buffer.t

  method line =
    match la0 with
    | Code 0x20 (* *) | Code 0x9 (*\t*) | Code 0x23 (*#*)  -> self#skip

  method lbrac =
    match la0 with
    | Code 0x5b (*[*) ->
      self#ignore
    | _ ->
      raise ParseError

  method rbrac =
    match la0 with
    | Code 0x5d (*[*) ->
      self#ignore
    | _ ->
      raise ParseError

  method change =
    self#name;
    (match la0 with
    | Code 0x5b (*[*) ->
      self#lbrac;
      self#requires_conflicts;
      self#rbrac);



  method requires_conflicts =


  method name =
    match la0 with
    | Code c when punct c ->
      raise ParseError
    | Code _ ->
      self#read;
      self#name_rest

  method name_rest =
    match la1 with
    | Code 7e (*~*) | Code 2f (*/*) | Code 3d (*=*) | Code 0x25 (*%*) | Code 0x5e (*^*)
    | Code 0x20 (* *) | Code 0x9 (*\t*) | Code 0x0a (*\n*)
    | Code c when digit c ->
      (match la0 with
       | Code c when punct c ->
         raise ParseError
       | Code _ ->
         self#get_buffer)
    | Code _ ->
      self#read

  method note =
    match la0 with
    | Code 0x20 (* *) | Code 0x9 (*\t*) ->
      self#ignore;
      self#skip
    | Code 0x23 (*#*) ->
      self#ignore;
      self#note
    | Code 0x0a (*\n*) ->
      self#ignore;
      None

  method note =
    match la0 with
    | Code 0x0a (*\n*) ->
      self#ignore;
      Some self#get_buffer
    | Code _ ->
      self#read;
      self#note

  method private get_buffer =
    let s = Buffer.contents in
    Buffer.clear;
    s

  method private read =
    self#get_code false

  method private ignore =
    self#get_code true

  method private get_code save =
    match Uutf.decode decoder with
    | `Uchar c ->
      if save then
        Buffer.add_utf_8_uchar la0 buffer;
      la0 <- la1
      la1 <- Code (Uchar.to_int c)
    | `End -> Eof
end

let parse_line dec =
  match Uutf.decode dec with
  | `Uchar c ->
    (match Uchar.to_int c with
     | 0x20
     | 0x9 -> 
     | 
