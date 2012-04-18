(** Types *)

type uri = Rdf_uri.uri

type world =
  { wld_graph : Rdf_graph.graph ;
    wld_prefix : uri ;
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
