(** The type of a Jabber ID *)

module Bare : sig
  type t [@@deriving sexp, ord]

  val of_string : string -> t
end

module Full : sig
  type t [@@deriving sexp, ord]

  val to_bare : t -> Bare.t
  val of_string : string -> t
  val to_string : t -> string
  val set_resource : string -> t -> t
end

module Domain : sig
  type t [@@deriving sexp]
end

type t =
  | Full_JID of Full.t
  | Bare_JID of Bare.t
  | Domain of Domain.t
[@@deriving sexp]

val set_resource : string -> t -> t
val anon : unit -> string
val to_bare_raw : t -> Bare.t
val to_bare : t -> t

(** [of_string s] creates a new jid from the string, splitting it appropriately *)
val of_string : string -> t

(** [to_string t] returns the string representation of t *)
val to_string : t -> string

val create_resource : unit -> string
