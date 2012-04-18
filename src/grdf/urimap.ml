(** *)

include Map.Make(struct type t = Rdf_uri.uri let compare = Rdf_uri.compare end)