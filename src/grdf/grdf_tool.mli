(** Defining and accessing tools. *)


val tools : Grdf_types.world -> string list

val add_tool : Grdf_types.world -> string -> string -> string
val name : Grdf_types.world -> string -> string
