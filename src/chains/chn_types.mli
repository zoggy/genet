(** Basic types for chains. *)

type user = string
type version_id = string

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

type fchain_name
val fchain_id : fchain_name -> version_id option
val fchain_chainname : fchain_name -> chain_name
val fchain_modname : fchain_name -> chain_modname
val fchain_basename : fchain_name -> chain_basename
val mk_fchain_name : chain_name -> version_id -> fchain_name

val uri_fchain_module : Grdf_types.uri -> chain_modname -> Grdf_types.uri
val uri_fchain : Grdf_types.uri -> fchain_name -> Grdf_types.uri

val is_uri_fchain_module : Grdf_types.uri -> Grdf_types.uri -> chain_modname option
val is_uri_fchain : Grdf_types.uri -> Grdf_types.uri -> fchain_name option

type ichain_name
val ichain_id : ichain_name -> string
val ichain_chainname : ichain_name -> chain_name
val ichain_modname : ichain_name -> chain_modname
val ichain_basename : ichain_name -> chain_basename
val mk_ichain_name : chain_name -> string -> ichain_name

val uri_ichain_module : Grdf_types.uri -> chain_modname -> Grdf_types.uri
val uri_ichain : Grdf_types.uri -> ichain_name -> Grdf_types.uri

val uri_inst_opn_of_flat_opn :
  prefix: Grdf_types.uri -> inst: Grdf_types.uri -> flat: Grdf_types.uri
  -> Grdf_types.uri

val uri_inst_port_of_flat_port :
  context -> inst: Grdf_types.uri -> flat: Grdf_types.uri -> Grdf_types.uri
