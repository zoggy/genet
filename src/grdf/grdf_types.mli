(** Types *)

type world =
  { wld_world : Rdf_types.world ;
    wld_model : Rdf_types.model ;
    wld_storage : Rdf_types.storage ;
    wld_rasqal : Rdf_types.rasqal_world ;
  }

type error =
  | Tool_exists of string
  | Parent_is_not_tool_or_branch
  | Branch_exists of string

exception Error of error

val error : error -> 'a
val string_of_error : error -> string
