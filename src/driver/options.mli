(** Common options for drivers. *)

type option_spec = string * Arg.spec * string

(** [option_version prefix] returns an option spec to print
   the given prefix followed by " version ...". *)

val option_version : string -> option_spec

val option_config : option_spec

val option_rdfxml : option_spec
val option_ntriples : option_spec

type option_values = {
  config_file : string  ; (** config filename *)
  args : string list ; (** arguments *)
  rdf_output_format : string ; (** format to use when output rdf *)
  }

(** [parse options] parses the command line using the given option specifications.
     @return the values of common options. *)
val parse : option_spec list -> option_values

(** [parse_command command] parses the command line according to the given
  specification or type {!Cmdline.command} .
     @return the values of common options. *)
val parse_command : Cmdline.command -> option_values







