(** Common options for drivers. *)

type option_spec = string * Arg.spec * string

(** [option_version prefix] returns an option spec to print
   the given prefix followed by " version ...". *)

val option_version : string -> option_spec
