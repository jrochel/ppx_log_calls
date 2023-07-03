module type PpxLogCallsAux = sig
  val log : (Format.formatter -> unit) -> unit
  val const : string -> Format.formatter -> 'a -> unit
  val unsupported : string -> Format.formatter -> 'a -> unit
  val pp_open_box : Format.formatter -> unit
  val pp_close_box : Format.formatter -> unit

  val pp_pbox :
     (Format.formatter -> 'a -> unit) ->
    Format.formatter ->
    'a ->
    unit

  val pp_tuple : Format.formatter -> (Format.formatter -> unit) list -> unit
  val e : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c

  val pp_tuple2 :
     (Format.formatter -> 'a -> unit) ->
    (Format.formatter -> 'b -> unit) ->
    Format.formatter ->
    'a * 'b ->
    unit

  val pp_tuple3 :
     (Format.formatter -> 'a -> unit) ->
    (Format.formatter -> 'b -> unit) ->
    (Format.formatter -> 'c -> unit) ->
    Format.formatter ->
    'a * 'b * 'c ->
    unit

  val pp_tuple4 :
     (Format.formatter -> 'a -> unit) ->
    (Format.formatter -> 'b -> unit) ->
    (Format.formatter -> 'c -> unit) ->
    (Format.formatter -> 'd -> unit) ->
    Format.formatter ->
    'a * 'b * 'c * 'd ->
    unit

  val pp_tuple5 :
     (Format.formatter -> 'a -> unit) ->
    (Format.formatter -> 'b -> unit) ->
    (Format.formatter -> 'c -> unit) ->
    (Format.formatter -> 'd -> unit) ->
    (Format.formatter -> 'e -> unit) ->
    Format.formatter ->
    'a * 'b * 'c * 'd * 'e ->
    unit

  val pp_tuple6 :
     (Format.formatter -> 'a -> unit) ->
    (Format.formatter -> 'b -> unit) ->
    (Format.formatter -> 'c -> unit) ->
    (Format.formatter -> 'd -> unit) ->
    (Format.formatter -> 'e -> unit) ->
    (Format.formatter -> 'f -> unit) ->
    Format.formatter ->
    'a * 'b * 'c * 'd * 'e * 'f ->
    unit

  val pp_tuple7 :
     (Format.formatter -> 'a -> unit) ->
    (Format.formatter -> 'b -> unit) ->
    (Format.formatter -> 'c -> unit) ->
    (Format.formatter -> 'd -> unit) ->
    (Format.formatter -> 'e -> unit) ->
    (Format.formatter -> 'f -> unit) ->
    (Format.formatter -> 'g -> unit) ->
    Format.formatter ->
    'a * 'b * 'c * 'd * 'e * 'f * 'g ->
    unit

  val pp_tuple8 :
     (Format.formatter -> 'a -> unit) ->
    (Format.formatter -> 'b -> unit) ->
    (Format.formatter -> 'c -> unit) ->
    (Format.formatter -> 'd -> unit) ->
    (Format.formatter -> 'e -> unit) ->
    (Format.formatter -> 'f -> unit) ->
    (Format.formatter -> 'g -> unit) ->
    (Format.formatter -> 'h -> unit) ->
    Format.formatter ->
    'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h ->
    unit
end

module type Conf = sig
  val log : (Format.formatter -> unit) -> unit
  val parentheses : [`Always | `None | `Smart]
end

module type S = sig
  module PpxLogCallsAux : PpxLogCallsAux

  val pp_bool : Format.formatter -> bool -> unit
  val pp_int : Format.formatter -> int -> unit
  val pp_float : Format.formatter -> float -> unit
  val pp_char : Format.formatter -> char -> unit
  val pp_string : Format.formatter -> string -> unit

  val pp_option :
     (Format.formatter -> 'a -> unit) ->
    Format.formatter ->
    'a option ->
    unit
end

module Make (C : Conf) = struct
  module PpxLogCallsAux = struct
    open Stdlib.Format

    let log = C.log
    let const str fmt _ = pp_print_string fmt str
    let unsupported str = const @@ "unsupported:" ^ str
    let pp_open_box fmt = pp_open_box fmt 2
    let pp_close_box fmt = pp_close_box fmt ()

    let pp_pbox pp fmt x =
      match C.parentheses with
      | `None -> pp fmt x
      | `Always -> Format.fprintf fmt "(@[%a@])" pp x
      | `Smart ->
        let preview =
          let buf = Buffer.create 9 in
          let fmt = Format.formatter_of_buffer buf in
          pp fmt x;
          Format.pp_print_flush fmt ();
          Buffer.contents buf
        in
        let produces_spaces = String.contains preview ' ' in
        let is_parenthesised =
          try
            let first = String.get preview 0
            and last = String.get preview (String.length preview - 1) in
            List.mem (first, last) ['[', ']'; '(', ')'; '{', '}'; '<', '>']
          with Invalid_argument _ -> false
        in
        if produces_spaces && not is_parenthesised
        then Format.fprintf fmt "(@[%a@])" pp x
        else pp fmt x

    let pp_tuple fmt pp_elts =
      let first = ref true in
      pp_print_char fmt '(';
      List.iter
        (fun f ->
          if !first then first := false else Format.fprintf fmt ",@ ";
          pp_open_box fmt;
          f fmt;
          pp_close_box fmt)
        pp_elts;
      pp_print_char fmt ')'

    let e pp e fmt = pp fmt e
    let pp_tuple2 pp1 pp2 fmt (e1, e2) = pp_tuple fmt [e pp1 e1; e pp2 e2]

    let pp_tuple3 pp1 pp2 pp3 fmt (e1, e2, e3) =
      pp_tuple fmt [e pp1 e1; e pp2 e2; e pp3 e3]

    let pp_tuple4 pp1 pp2 pp3 pp4 fmt (e1, e2, e3, e4) =
      pp_tuple fmt [e pp1 e1; e pp2 e2; e pp3 e3; e pp4 e4]

    let pp_tuple5 pp1 pp2 pp3 pp4 pp5 fmt (e1, e2, e3, e4, e5) =
      pp_tuple fmt [e pp1 e1; e pp2 e2; e pp3 e3; e pp4 e4; e pp5 e5]

    let pp_tuple6 pp1 pp2 pp3 pp4 pp5 pp6 fmt (e1, e2, e3, e4, e5, e6) =
      pp_tuple fmt [e pp1 e1; e pp2 e2; e pp3 e3; e pp4 e4; e pp5 e5; e pp6 e6]

    let pp_tuple7 p1 p2 p3 p4 p5 p6 p7 fmt (e1, e2, e3, e4, e5, e6, e7) =
      pp_tuple fmt
        [e p1 e1; e p2 e2; e p3 e3; e p4 e4; e p5 e5; e p6 e6; e p7 e7]

    let pp_tuple8 p1 p2 p3 p4 p5 p6 p7 p8 fmt (e1, e2, e3, e4, e5, e6, e7, e8) =
      pp_tuple fmt
        [e p1 e1; e p2 e2; e p3 e3; e p4 e4; e p5 e5; e p6 e6; e p7 e7; e p8 e8]
  end

  module PpxLogCallsStdTypes = struct
    open Format

    let pp_bool = pp_print_bool
    let pp_int = pp_print_int
    let pp_float = pp_print_float
    let pp_char fmt c = fprintf fmt "'%c'" c
    let pp_string fmt str = fprintf fmt "\"%s\"" str

    let pp_option pp fmt = function
      | None -> pp_print_string fmt "None"
      | Some x -> fprintf fmt "Some @[%a@]" pp x
  end

  include PpxLogCallsStdTypes
end

module Stdout = Make (struct
  let log f = f Format.std_formatter let parentheses = `Smart
end)
