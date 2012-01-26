(** *)

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

let error e = raise (Error e);;

let string_of_error = function
| Tool_exists name -> Printf.sprintf "Tool %s already exists" name
| Parent_is_not_tool_or_branch -> "Parent is neither a tool nor a branch"
| Branch_exists name -> Printf.sprintf "Branch %s already exists" name
;;
