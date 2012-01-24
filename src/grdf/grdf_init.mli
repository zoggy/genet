(** Creating storage. *)

val open_storage : Config.t -> (Rdf_types.model * Rdf_types.world)

val init : Rdf_types.world -> Rdf_types.model -> string -> unit