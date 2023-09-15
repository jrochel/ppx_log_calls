(** A collection of pretty-printing functions used by the PPX-generated code.
    To be ignored by the average user of this library. *)
module type PpxLogCallsAux = sig
  val log : (Format.formatter -> unit) -> unit
  val log_cut_flush : (Format.formatter -> unit) -> unit
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
  (** The strategy with which parentheses are placed around pretty-printed
      elements. Note that the strategy [`Always] incurs significant overhead. *)
end

module type S = sig
  module PpxLogCallsAux : PpxLogCallsAux
  (** Not pertinent for the normal user *)

  (** Below are some pretty-printing functions for standard types *)

  val pp_unit : Format.formatter -> unit -> unit
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

  val pp_list :
     (Format.formatter -> 'a -> unit) ->
    Format.formatter ->
    'a list ->
    unit
end

(** Example usage:
    {[
    open Log_calls.Make (struct
      let log f = f Format.std_formatter
      let parentheses = `Smart
    end)
    ]}
    You have to open the module in order for the pretty-printing functions
    [pp_char], [pp_string], etc. as well as the [PpxLogCallsAux] module to be visible.
*)
module Make : functor (_ : Conf) -> S

module Stdout : S
