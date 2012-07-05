(** Access to genet main configuration file. *)

type t = {
  project_name : string;
  project_id : string;
  db_engine : string;
  db_name : string;
  db_user : string;
  db_passwd : string;
  db_host : string;
  rest_api : Rdf_uri.uri ;
  root_dir : string;
}
val read_config : string -> t
val string_of_config : t -> string

val in_dir : t -> string
val out_dir : t -> string
val chains_dir : t -> string
val data_dir : t -> string
val web_dir : t -> string
