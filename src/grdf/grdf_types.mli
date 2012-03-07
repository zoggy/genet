(** Types *)

type uri = string

type world =
  { wld_world : Rdf_types.world ;
    wld_model : Rdf_types.model ;
    wld_storage : Rdf_types.storage ;
    wld_rasqal : Rdf_types.rasqal_world ;
    wld_prefix : string ;
  }

type error =
  | Tool_exists of string
  | Branch_exists of string
  | Version_exists of string
  | Intf_exists of string
  | Filetype_exists of string
  | Not_tool_or_branch of uri
  | Not_a_tool of uri
  | Not_a_version of uri
  | Tool_of_branch_differs of uri * uri * uri

exception Error of error

val error : error -> 'a
val string_of_error : error -> string
