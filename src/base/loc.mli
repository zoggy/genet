(** Lexing locations. *)

type t = { loc_start: Lexing.position; loc_end: Lexing.position }

val dummy : t

type problem = t Checks.problem

exception Problem of problem

val raise_problem : t -> string -> 'a

val string_of_problem : problem -> string

val string_of_loc : t -> string
val string_of_problem : t Checks.problem -> string
