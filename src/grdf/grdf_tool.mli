(** Defining and accessing tools. *)

type t = { tool_name : string; tool_uri : string; }

val tools : Grdf_types.world -> t list

val get_tool : Grdf_types.world -> string -> t option

val add_tool : Grdf_types.world -> string -> string -> t option
