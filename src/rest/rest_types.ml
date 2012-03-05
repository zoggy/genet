(** *)

type arg = string * string list

type met =
  | Get of Grdf_types.uri * arg list
  | Delete of Grdf_types.uri
  | Post of Grdf_types.uri * Yojson.Basic.json
  | Put of Grdf_types.uri * Yojson.Basic.json
;;

