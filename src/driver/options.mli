(** Common options for drivers. *)

type option_spec = string * Arg.spec * string

(** [option_version prefix] returns an option spec to print
   the given prefix followed by " version ...". *)

val option_version : string -> option_spec

val option_config : option_spec

type option_values = {
  config_file : string  ; (** config filename *)
  args : string list ; (** arguments *)
  }

(** [parse options] parses the command line using the given option specifications.
     @return the list of remaining arguments. *)
val parse : option_spec list -> option_values







