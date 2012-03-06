(** *)

type arg = string * string
type uri = Grdf_types.uri

type met =
  | Get of Grdf_types.uri * arg list
  | Delete of Grdf_types.uri
  | Post of Grdf_types.uri * Yojson.Basic.json
  | Put of Grdf_types.uri * Yojson.Basic.json
;;

type content_type = Xhtml | Json

type thing =
  | Tool of uri
  | Branch of uri
  | Version of uri
  | Intf of uri
  | Other of string
;;

type response = arg list * string

type user = string

type context =
  { ctx_rdf: Grdf_types.world ;
    ctx_cfg: Config.t ;
    ctx_user : user option ;
  }

type get_handler = thing -> arg list -> context -> response
type delete_handler = thing -> context -> response
type post_handler = thing -> Yojson.Basic.json -> context -> response
type put_handler = thing -> Yojson.Basic.json -> context -> response

type content_type_handlers = {
  h_get : get_handler ;
  h_del : delete_handler ;
  h_post : post_handler ;
  h_put : put_handler ;
  }
