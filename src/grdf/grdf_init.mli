(** Creating storage. *)

val open_storage : Config.t -> Grdf_types.world

val init : Grdf_types.world -> string -> unit
val close : Grdf_types.world -> unit
