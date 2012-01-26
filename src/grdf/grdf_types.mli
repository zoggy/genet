(** Types *)

type world =
  { wld_world : Rdf_types.world ;
    wld_model : Rdf_types.model ;
    wld_storage : Rdf_types.storage ;
    wld_rasqal : Rdf_types.rasqal_world ;
  }

type error =
  | Tool_exists of string
  | Branch_exists of string
  | Version_exists of string
  | Interface_exists of string
  | Parent_is_not_tool_or_branch
  | Not_a_tool of string
  | Not_a_version of string
  | Tool_of_branch_differs of string * string * string

exception Error of error

val error : error -> 'a
val string_of_error : error -> string
