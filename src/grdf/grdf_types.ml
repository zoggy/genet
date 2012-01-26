(** *)

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

let error e = raise (Error e);;

let string_of_error = function
| Tool_exists name -> Printf.sprintf "Tool %s already exists." name
| Branch_exists name -> Printf.sprintf "Branch %s already exists." name
| Version_exists name -> Printf.sprintf "Version %s already exists." name
| Interface_exists name -> Printf.sprintf "Interface %s already exists." name
| Parent_is_not_tool_or_branch -> "Parent is neither a tool nor a branch."
| Not_a_tool uri -> Printf.sprintf "%s is not a tool." uri
| Not_a_version uri -> Printf.sprintf "%s is not a version." uri
| Tool_of_branch_differs (branch, branch_tool, tool) ->
    Printf.sprintf "The tool of branch %s is %s but %s was expected."
    branch branch_tool tool
;;
