(** Defining and accessing tools. *)


val tools : Grdf_types.world -> Grdf_types.uri list

val add_tool : Grdf_types.world -> string -> Grdf_types.uri
val name : Grdf_types.world -> Grdf_types.uri -> string
