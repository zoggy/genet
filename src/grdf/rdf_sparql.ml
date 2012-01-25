(** *)

type varname = string
type projection = varname list

type select_query = string
type construct_query = string
type ask_query = string
type describe_query = string

type query =
  Select of select_query
| Construct of construct_query
| Ask of ask_query
| Describe of describe_query

