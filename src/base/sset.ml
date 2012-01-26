(** *)

include Set.Make(struct type t = string let compare = Pervasives.compare end)