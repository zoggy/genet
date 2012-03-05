(** *)

open Rest_types;;

let query rdf_wld = function
  Get (uri, args) -> ()
| Delete uri -> ()
| Post (uri, json) -> ()
| Put (uri, json) -> ()
;;
  