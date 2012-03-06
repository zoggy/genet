(** *)

open Rest_types;;

exception Not_implemented of string;;
let not_implemented msg = raise (Not_implemented msg);;

let xhtml_handlers =
  { h_get = (fun _ _ _ -> not_implemented "xhtml get") ;
    h_del = (fun _ _ -> not_implemented "xhtml delete");
    h_post = (fun _ _ -> not_implemented "xhtml post");
    h_put = (fun _ _ -> not_implemented "xhtml put");
  }

let json_handlers =
  { h_get = (fun _ _ _ -> not_implemented "json get") ;
    h_del = (fun _ _ -> not_implemented "json delete");
    h_post = (fun _ _ -> not_implemented "json post");
    h_put = (fun _ _ -> not_implemented "json put");
  }


let thing_of_uri uri = Tool uri;;

let handler_by_method h = function
  Get (uri, args) -> h.h_get (thing_of_uri uri) args
| Delete uri -> h.h_del (thing_of_uri uri)
| Post (uri, json) -> h.h_post (thing_of_uri uri) json
| Put (uri, json) -> h.h_put (thing_of_uri uri) json

let query content_type =
  match content_type with
    Xhtml -> handler_by_method xhtml_handlers
  | Json -> handler_by_method json_handlers
;;
