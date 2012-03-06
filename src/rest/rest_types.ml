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
  | Filetype of uri
  | Static_file of string * string
  | Other of string
;;

type response = arg list * string

type user = string

type context =
  { ctx_rdf: Grdf_types.world ;
    ctx_cfg: Config.t ;
    ctx_user : user option ;
  }

type get_handler = context -> thing -> arg list -> response
type delete_handler = context -> thing -> response
type post_handler = context -> thing -> Yojson.Basic.json -> response
type put_handler = context -> thing -> Yojson.Basic.json -> response

type content_type_handlers = {
  h_get : get_handler ;
  h_del : delete_handler ;
  h_post : post_handler ;
  h_put : put_handler ;
  }
