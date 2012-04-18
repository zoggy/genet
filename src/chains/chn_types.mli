(** Basic types for chains. *)

type user = string

type context =
  { ctx_rdf: Grdf_types.world ;
    ctx_cfg: Config.t ;
    ctx_user : user option ;
  }

type chain_modname
type chain_basename
type chain_name
val chain_modname : chain_name -> chain_modname
val chain_basename : chain_name -> chain_basename
val mk_chain_name : chain_modname -> chain_basename -> chain_name
val string_of_chain_modname : chain_modname -> string
val string_of_chain_basename : chain_basename -> string
val string_of_chain_name : chain_name -> string
val chain_modname_of_string : string -> chain_modname
val chain_basename_of_string : string -> chain_basename
val chain_name_of_string : string -> chain_name

val compare_chain_modname : chain_modname -> chain_modname -> int
val compare_chain_basename : chain_basename -> chain_basename -> int
val compare_chain_name : chain_name -> chain_name -> int

val uri_chain_module : Grdf_types.uri -> chain_modname -> Grdf_types.uri
val uri_chain : Grdf_types.uri -> chain_name -> Grdf_types.uri

val is_uri_chain_module : Grdf_types.uri -> Grdf_types.uri -> string option
val is_uri_chain : Grdf_types.uri -> Grdf_types.uri -> string option
val uri_intf_of_interface_spec : prefix: Grdf_types.uri -> string -> Grdf_types.uri

val uri_fchain_module : Grdf_types.uri -> chain_modname -> Grdf_types.uri
val uri_fchain : Grdf_types.uri -> chain_name -> string -> Grdf_types.uri

val is_uri_fchain_module : Grdf_types.uri -> Grdf_types.uri -> chain_modname option
val is_uri_fchain : Grdf_types.uri -> Grdf_types.uri -> (chain_name * string) option

