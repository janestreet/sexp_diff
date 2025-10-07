@@ portable

open Core

(** Expose display helpers for use by the incr_dom-friendly library "sexp_diff" *)
module Color : sig
  type t =
    | Red
    | Green
    | Plain
  [@@deriving sexp_of, compare]

  val equal : t -> t -> bool
end

module Line : sig
  type t =
    { color : Color.t
    ; content : string
    }
  [@@deriving sexp_of]

  val to_text
    :  green:(string -> 'a)
    -> red:(string -> 'a)
    -> plain:(string -> 'a)
    -> t
    -> 'a

  val length : t -> int
end

module (Display_options @@ nonportable) : sig @@ portable
  type t : immutable_data [@@deriving sexp_of]

  val create
    :  ?collapse_threshold:int
    -> ?include_num_unchanged_lines:bool
    -> ?num_shown:int
    -> unit
    -> t

  val default : t

  (** flags compatible with `man diff` *)
  val param : t Command.Param.t @@ nonportable
end

module Line_pair : sig
  type t =
    | Same of Line.t
    | Different of (Line.t * Line.t)

  val fst : t -> Line.t
  val snd : t -> Line.t
end

module Hideable_line_pair : sig
  type t =
    | Line_pair of Line_pair.t
    | Hidden of int option
    | All_hidden
end

val hideable_line_pairs
  :  ?display_options:Display_options.t
  -> Diff.t
  -> Hideable_line_pair.t list
