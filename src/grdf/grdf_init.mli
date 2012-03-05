(** Creating model. *)

val open_storage : Config.t -> Grdf_types.world

val init : Grdf_types.world -> unit
val close : Grdf_types.world -> unit
