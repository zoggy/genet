(** Types for REST API. *)

type arg = string * string

type uri = Grdf_types.uri
type path = string

type met =
  | Get of path * arg list
  | Delete of path
  | Post of path * Yojson.Basic.json
  | Put of path * Yojson.Basic.json
;;

type content_type = Xhtml | Json

type thing =
  | Tool of uri
  | Branch of uri
  | Version of uri
  | Intf of uri
  | Filetype of uri
  | Tools
  | Branches of uri (** uri of tool *)
  | Versions of uri (** uri of tool or branch *)
  | Intfs of uri (** uri of tool or branch *)
  | Filetypes
  | Static_file of string * string
  | Other of string
  | Chains
  | Chain_module of Chn_types.chain_modname
  | Chain of Chn_types.chain_name
  | Flat_chains
  | Flat_chain_module of Chn_types.chain_modname
  | Flat_chain of Chn_types.chain_name * string
;;

type response = arg list * string

type user = Chn_types.user

type context = Chn_types.context =
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

