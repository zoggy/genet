(** Basic types for chains. *)

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

val uri_chain_module : string -> chain_modname -> Grdf_types.uri
val uri_chain : string -> chain_name -> Grdf_types.uri

val is_uri_chain_module : string -> Grdf_types.uri -> string option
val is_uri_chain : string -> Grdf_types.uri -> string option
